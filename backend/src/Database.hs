{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Database where

import Control.Monad (when, (<=<))
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Control.Retry (RetryStatus (..), exponentialBackoff, recoverAll)
import Data.ByteString (ByteString)
import Data.ClientRequest (Contacts (..), Office, Registration (..), RegistrationId (..), UserRegistration (..), registrationDate)
import Data.Env (Env (..))
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty (NonEmpty ((:|)), groupWith, toList)
import Data.Maybe (listToMaybe)
import Data.Pool (Pool, createPool, withResource)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime)
import Data.User (Admin, AdminUser, Email, OpenIdUser, User (..))
import Database.PostgreSQL.Simple (Connection, FromRow, In (..), Only (..), Query, ToRow, begin, close, commit, connectPostgreSQL, execute, execute_, query, query_, returning, rollback, withTransaction)
import System.Environment (lookupEnv)

queryRegistrations :: (MonadIO m, MonadReader Env m) => Email -> Day -> Day -> m [Registration]
queryRegistrations email start end =
  query'
    "SELECT * FROM registrations WHERE user_email = ? AND date >= ? AND date <= ? ORDER BY date DESC"
    (email, start, end)

queryBooked :: (MonadIO m, MonadReader Env m) => Text -> Day -> Day -> m [Contacts]
queryBooked office start end =
  query'
    ( withUserFields "r.date, "
        <> " RIGHT JOIN registrations AS r ON u.user_email = r.user_email WHERE r.office = ? AND r.date >= ? AND r.date <= ?"
        <> " ORDER BY r.date DESC"
    )
    (office, start, end)
    <&> fmap toContacts . groupWith fst'
  where
    fst' (x, _, _, _, _, _) = x
    toContacts ys@((d, _, _, _, _, _) :| _) = MkContacts d office . fmap toUser $ toList ys
    toUser (_, name, email, picture, o, isAdmin) = MkUser name email picture o isAdmin

queryContacts :: (MonadIO m, MonadReader Env m) => Email -> Day -> Day -> m [Contacts]
queryContacts email start end =
  query'
    "SELECT office, date FROM registrations WHERE workmode = 'Office' AND user_email = ? AND date >= ? AND date <= ? ORDER BY date DESC"
    (email, start, end)
    >>= mapM
      ( \t@(office, date) ->
          MkContacts date office
            <$> query'
              ( userFields <> " WHERE u.user_email IN ("
                  <> "SELECT r.user_email FROM registrations AS r WHERE workmode = 'Office' AND office = ? AND date = ?"
                  <> ")"
              )
              t
      )

tryRegistrations :: (MonadIO m, MonadReader Env m) => Maybe AdminUser -> Email -> [Registration] -> m (Maybe [Day])
tryRegistrations admin user xs = do
  conns <- asks pool
  liftIO $ withResource conns \con -> do
    begin con
    let allowPast = maybe " AND EXCLUDED.date >= CURRENT_DATE " (const " ") admin

    (updatedDays :: [Day]) <-
      fmap fromOnly
        <$> returning
          con
          ( "INSERT INTO registrations AS r (user_email, office, date, workmode, confirmed, client_name) VALUES (?, ?, ?, ?, ?, ?) "
              <> "ON CONFLICT (user_email, date) DO UPDATE SET office = EXCLUDED.office, workmode = EXCLUDED.workmode, "
              <> "confirmed = EXCLUDED.confirmed, client_name = EXCLUDED.client_name "
              <> "WHERE (r.confirmed IS NULL OR r.confirmed = false)"
              <> allowPast
              <> "RETURNING r.date"
          )
          (fmap (MkUserRegistration user) xs)

    (overfullDays :: [Day]) <-
      fmap fromOnly
        <$> queryIO
          conns
          ( "SELECT date FROM ("
              <> "SELECT r.date, o.capacity, count(*) AS x FROM registrations AS r LEFT JOIN offices AS o ON r.office = o.name "
              <> "WHERE r.workmode = 'Office' AND r.date IN ? BY r.date, o.capacity"
              <> ") AS res WHERE res.capacity < res.x"
          )
          (Only . In $ fmap registrationDate xs)

    let errorDays = overfullDays ++ filter (not . (`elem` updatedDays)) (fmap registrationDate xs)
    case errorDays of
      [] -> commit con $> Nothing
      errs -> rollback con $> Just errs

confirmRegistration :: (MonadIO m, MonadReader Env m) => RegistrationId -> m Bool
confirmRegistration MkRegistrationId {date, email} =
  query'
    "UPDATE registrations SET confirmed = true WHERE user_email = ? AND date = ? RETURNING user_email"
    (email, date)
    <&> \case
      [] -> False
      (_ :: [Only Text]) -> True

deleteRegistration :: (MonadIO m, MonadReader Env m) => RegistrationId -> m (Maybe RegistrationId)
deleteRegistration rid@MkRegistrationId {email, date} =
  query'
    "DELETE FROM registrations WHERE user_email = ? AND date = ? RETURNING user_email"
    (email, date)
    <&> \case
      [] -> Just rid
      (_ :: [Only Text]) -> Nothing

getOffices :: (MonadIO m, MonadReader Env m) => m [Office]
getOffices = query'_ "SELECT * FROM offices"

setOffice :: (MonadIO m, MonadReader Env m) => Office -> m ()
setOffice =
  exec $
    "INSERT INTO offices (name, capacity) VALUES (?, ?)"
      <> " ON CONFLICT (name) DO UPDATE SET capacity = EXCLUDED.capacity"

deleteOffice :: (MonadIO m, MonadReader Env m) => Text -> m (Maybe Office)
deleteOffice = pure . listToMaybe <=< query' "DELETE FROM offices WHERE name = ? RETURNING *" . Only

saveUser :: (MonadIO m, MonadReader Env m) => OpenIdUser -> m ()
saveUser =
  exec $
    "INSERT INTO users (name, user_email, picture, default_office, expire_date, access_token, refresh_token, id_token) VALUES (?,?,?,?,?,?,?,?) "
      <> "ON CONFLICT (user_email) DO UPDATE "
      <> "SET name = EXCLUDED.name, picture = EXCLUDED.picture, expire_date = EXCLUDED.expire_date, "
      <> "access_token = EXCLUDED.access_token, refresh_token = EXCLUDED.refresh_token, id_token = EXCLUDED.id_token"

getUsers :: (MonadIO m, MonadReader Env m) => m [User]
getUsers = query'_ userFields

withUserFields :: Query -> Query
withUserFields q =
  "SELECT " <> q <> "u.name, u.user_email, u.picture, u.default_office, CASE WHEN admins.user_email IS NULL THEN false ELSE true END AS is_admin"
    <> " FROM users AS u LEFT JOIN admins ON u.user_email = admins.user_email"

userFields :: Query
userFields = withUserFields ""

getAdmins :: (MonadIO m, MonadReader Env m) => Maybe Text -> m [Admin]
getAdmins = \case
  Nothing -> query'_ q
  Just office -> query' (q <> " WHERE u.default_office = ?") (Only office)
  where
    q = "SELECT u.name, u.user_email FROM users AS u INNER JOIN admins ON u.user_email = admins.user_email"

putAdmin :: (MonadIO m, MonadReader Env m) => Email -> m ()
putAdmin = exec "INSERT INTO admins (user_email) VALUES (?) ON CONFLICT (user_email) DO NOTHING" . Only

removeAdmin :: (MonadIO m, MonadReader Env m) => Email -> m (Maybe Email)
removeAdmin email = listToMaybe . fmap fromOnly <$> query' "DELETE FROM admins WHERE user_email = ? RETURNING user_email" (Only email)

setDefaultOffice :: (MonadIO m, MonadReader Env m) => User -> Text -> m (Maybe User)
setDefaultOffice MkUser {email} office =
  listToMaybe
    <$> query'
      ( "UPDATE users SET default_office = sub.office_name "
          <> "FROM ("
          <> sub
          <> ") sub "
          <> "WHERE users.user_email = ? AND sub.user_email = users.user_email "
          <> "RETURNING users.name, users.user_email, users.picture, users.default_office, sub.is_admin"
      )
      (office, email)
  where
    sub =
      "SELECT u.user_email, offices.name AS office_name, CASE WHEN admins.user_email IS NULL THEN false ELSE true END AS is_admin "
        <> "FROM users AS u "
        <> "LEFT JOIN admins ON u.user_email = admins.user_email "
        <> "INNER JOIN offices ON offices.name = ?"

updateAccessToken :: (MonadIO m, MonadReader Env m) => Text -> Text -> UTCTime -> Maybe Text -> Maybe Text -> m (Maybe User)
updateAccessToken oldRefreshToken accessToken expireDate idToken =
  fmap listToMaybe . \case
    Nothing -> query' (q "") (accessToken, expireDate, idToken, oldRefreshToken)
    Just refreshToken -> query' (q ", refresh_token = ?") (accessToken, expireDate, refreshToken, idToken, oldRefreshToken)
  where
    q x =
      "UPDATE users SET access_token = ?, expire_date = ?, id_token = ?"
        <> x
        <> " WHERE refresh_token = ? RETURNING name, user_email, picture, default_office, false"

logoutUser :: (MonadIO m, MonadReader Env m) => Email -> m (Maybe Text)
logoutUser userEmail =
  query'
    "UPDATE users SET access_token = NULL, refresh_token = NULL, expire_date = NULL WHERE user_email = ? RETURNING id_token"
    (Only userEmail)
    <&> \case
      [Only token] -> token
      _ -> Nothing

checkUser :: (MonadIO m, MonadReader Env m) => Text -> m (Maybe (Either Text User))
checkUser token =
  query' q (Only token) >>= \case
    [(userName, userEmail, picture, office, expire, _ :: Text, refreshToken, _ :: Maybe Text, admin)] ->
      liftIO getCurrentTime <&> \now ->
        if now > expire
          then Left <$> refreshToken
          else Just . Right $ MkUser userName userEmail picture office admin
    _ -> pure Nothing
  where
    q =
      "SELECT users.*, CASE WHEN admins.user_email IS NULL THEN false ELSE true END AS is_admin "
        <> "FROM users LEFT JOIN admins ON users.user_email = admins.user_email "
        <> "WHERE access_token = ?"

addAdmin :: (MonadIO m, MonadReader Env m) => Text -> m ()
addAdmin = exec "INSERT INTO admins (user_email) VALUES (?) ON CONFLICT (user_email) DO NOTHING" . Only

initDatabase :: MonadIO m => ByteString -> m (Pool Connection)
initDatabase connectionString = do
  pool <- liftIO $ createPool (retry "database" $ connectPostgreSQL connectionString) close 2 60 10
  _ <- liftIO . withResource pool $ \conn ->
    execute_
      conn
      ( "CREATE TABLE IF NOT EXISTS registrations ("
          <> "user_email text not null, "
          <> "office text not null, "
          <> "date date not null, "
          <> "workmode text not null, "
          <> "confirmed bool, "
          <> "client_name text, "
          <> "UNIQUE (user_email, date)"
          <> ")"
      )
  _ <- liftIO . withResource pool $ \conn ->
    execute_
      conn
      ( "CREATE TABLE IF NOT EXISTS offices ("
          <> "name text PRIMARY KEY, "
          <> "capacity integer not null"
          <> ")"
      )
  _ <- liftIO . withResource pool $ \conn ->
    execute_
      conn
      ( "CREATE TABLE IF NOT EXISTS users ("
          <> "name text not null, "
          <> "user_email text PRIMARY KEY, "
          <> "picture text not null, "
          <> "default_office text, "
          <> "expire_date timestamptz, "
          <> "access_token text, "
          <> "refresh_token text, "
          <> "id_token text"
          <> ")"
      )
  _ <- liftIO . withResource pool $ \conn -> execute_ conn "CREATE TABLE IF NOT EXISTS admins (user_email text PRIMARY KEY)"
  admin <- liftIO $ lookupEnv "INITIAL_ADMIN"
  _ <- case admin of
    Just x -> liftIO . withResource pool $ \conn ->
      execute conn "INSERT INTO admins (user_email) SELECT ? WHERE NOT EXISTS (SELECT * FROM admins)" (Only x)
    Nothing -> pure 0
  pure pool

exec :: (MonadIO m, MonadReader Env m, ToRow r) => Query -> r -> m ()
exec q r = do
  conns <- asks pool
  _ <- liftIO . withResource conns $ \conn -> execute conn q r
  pure ()

executeManyIO :: (ToRow r, FromRow x) => Pool Connection -> Query -> [r] -> IO [x]
executeManyIO conns q rs = withResource conns $ \conn -> returning conn q rs

query'_ :: (MonadIO m, MonadReader Env m, FromRow r) => Query -> m [r]
query'_ q = do
  conns <- asks pool
  liftIO . withResource conns $ \conn -> query_ conn q

query' :: (MonadIO m, MonadReader Env m, ToRow x, FromRow r) => Query -> x -> m [r]
query' q x = do
  conns <- asks pool
  liftIO . withResource conns $ \conn -> query conn q x

queryIO :: (FromRow r, ToRow x) => Pool Connection -> Query -> x -> IO [r]
queryIO conns q x = withResource conns $ \conn -> query conn q x

withTransaction' :: (MonadIO m, MonadReader Env m) => IO a -> m a
withTransaction' x = do
  conns <- asks pool
  liftIO . withResource conns $ \conn -> withTransaction conn x

retry :: (MonadIO m, MonadMask m) => String -> m a -> m a
retry msg x = recoverAll (exponentialBackoff 100) $ \RetryStatus {rsIterNumber} ->
  when
    (rsIterNumber > 0)
    (liftIO $ putStrLn $ "failed to connect to " <> msg <> ", retrying")
    *> x
