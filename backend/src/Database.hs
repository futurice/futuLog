{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Database where

import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Control.Retry (RetryStatus (..), exponentialBackoff, recoverAll)
import Data.ByteString (ByteString)
import Data.ClientRequest (Contact (..), Office, Registration (..), RegistrationId (..), UserRegistration (..), registrationDate)
import Data.Env (Env (..))
import Data.Functor (($>), (<&>))
import Data.List (groupBy, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, toList)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Pool (Pool, createPool, withResource)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime)
import Data.User (Admin, Email, OpenIdUser, User (..))
import Data.Workmode (Workmode (..))
import Database.PostgreSQL.Simple (Connection, FromRow, Only (..), Query, ToRow, close, connectPostgreSQL, execute, executeMany, execute_, query, query_, withTransaction)
import Database.PostgreSQL.Simple.Types (In (In))
import System.Environment (lookupEnv)

queryRegistrations :: (MonadIO m, MonadReader Env m) => Email -> Day -> Day -> m [Registration]
queryRegistrations email start end =
  query'
    "SELECT * FROM registrations WHERE user_email = ? AND date >= ? AND date <= ? ORDER BY date DESC"
    (email, start, end)

queryBooked :: (MonadIO m, MonadReader Env m) => Text -> Day -> m [User]
queryBooked office date =
  query'
    (userFields <> " RIGHT JOIN registrations AS r ON u.user_email = r.user_email WHERE r.office = ? AND r.date = ?")
    (office, date)

queryContacts :: (MonadIO m, MonadReader Env m) => Email -> Day -> Day -> m [Contact]
queryContacts email start end =
  query'
    "SELECT office, date FROM registrations WHERE workmode = 'Office' AND user_email = ? AND date >= ? AND date <= ? ORDER BY date DESC"
    (email, start, end)
    >>= mapM
      ( \t@(site, date) ->
          MkContact date site
            <$> query'
              ( userFields <> " WHERE user_email IN ("
                  <> "SELECT user_email FROM registrations WHERE workmode = 'Office' AND site = ? AND date = ?"
                  <> ")"
              )
              t
      )

tryRegistrations :: (MonadIO m, MonadReader Env m) => User -> [Registration] -> m (Maybe [Day])
tryRegistrations user xs = do
  conns <- asks pool
  withTransaction' do
    days <- mapM (queryOffice conns) $ groupRegistrations xs
    case concat days of
      [] -> executeManyIO conns "INSERT INTO registrations VALUES (?, ?, ?, ?, ?, ?)" (fmap (MkUserRegistration user) xs) $> Nothing
      ys -> pure $ Just ys
  where
    groupRegistrations = mapMaybe nonEmpty . groupBy (\a b -> office a == office b) . sortOn office
    queryOffice conns ys@(MkRegistration {office} :| _) =
      fmap fromOnly
        <$> queryIO
          conns
          ("SELECT date FROM (" <> subquery <> ") AS res WHERE res.capacity <= res.x OR res.date < current_date")
          (office, In (toList $ fmap registrationDate ys))
    subquery =
      "SELECT r.date, o.capacity, count(*) AS x FROM registrations AS r LEFT JOIN offices AS o ON r.office = o.name "
        <> "WHERE r.workmode = 'Office' AND r.office = ? AND r.date IN ? GROUP BY r.date, o.capacity"

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

saveRegistration :: (MonadIO m, MonadReader Env m) => Email -> Registration -> m ()
saveRegistration email MkRegistration {office, date, workmode} = do
  exec "DELETE FROM workmodes WHERE user_email = ? AND date = ?" (email, date)
  case workmode of
    Home -> mkSimpleQuery "Home"
    Leave -> mkSimpleQuery "Leave"
    (Client name) ->
      exec
        "INSERT INTO workmodes (user_email, office, date, workmode, client_name) VALUES (?, ?, ?, ?, ?)"
        (email, office, date, "Client" :: Text, name)
    (Office confirmed) ->
      exec
        "INSERT INTO workmodes (user_email, office, date, workmode, confirmed) VALUES (?, ?, ?, ?, ?)"
        (email, office, date, "Office" :: Text, confirmed)
  where
    mkSimpleQuery s =
      exec
        "INSERT INTO workmodes (user_email, office, date, workmode) VALUES (?, ?, ?, ?)"
        (email, office, date, s :: Text)

getOffices :: (MonadIO m, MonadReader Env m) => m [Office]
getOffices = query'_ "SELECT * FROM offices"

saveUser :: (MonadIO m, MonadReader Env m) => OpenIdUser -> m ()
saveUser =
  exec $
    "INSERT INTO users (name, user_email, picture, default_office, expire_date, access_token, refresh_token, id_token) VALUES (?,?,?,?,?,?,?,?) "
      <> "ON CONFLICT (user_email) DO UPDATE "
      <> "SET name = EXCLUDED.name, picture = EXCLUDED.picture, default_office = EXCLUDED.default_office, expire_date = EXCLUDED.expire_date, "
      <> "access_token = EXCLUDED.access_token, refresh_token = EXCLUDED.refresh_token, id_token = EXCLUDED.id_token"

getUsers :: (MonadIO m, MonadReader Env m) => m [User]
getUsers = query'_ userFields

userFields :: Query
userFields =
  "SELECT u.name, u.user_email, u.picture, u.default_office, CASE WHEN admins.user_email IS NULL THEN false ELSE true END AS is_admin"
    <> "FROM users AS u LEFT JOIN admins ON u.user_email = admins.user_email"

getAdmins :: (MonadIO m, MonadReader Env m) => Maybe Text -> m [Admin]
getAdmins = \case
  Nothing -> query'_ q
  Just office -> query' (q <> " WHERE u.default_office = ?") (Only office)
  where
    q = "SELECT u.name, u.user_email FROM users AS u INNER JOIN admins ON u.user_email = admins.user_email"

putAdmin :: (MonadIO m, MonadReader Env m) => Email -> m ()
putAdmin = exec "INSERT INTO admins (user_email) VALUES (?) ON CONFLICT (user_email) DO NOTHING" . Only

removeAdmin :: (MonadIO m, MonadReader Env m) => Email -> m (Maybe Email)
removeAdmin email = listToMaybe . fmap fromOnly <$> query' "DELETE FROM admins WHERE user_email = ?" (Only email)

setDefaultOffice :: (MonadIO m, MonadReader Env m) => User -> Text -> m (Maybe User)
setDefaultOffice MkUser {email} office =
  listToMaybe
    <$> query'
      ( "UPDATE users SET default_office = sub.office_name"
          <> "FROM ("
          <> sub
          <> ") sub"
          <> "WHERE users.user_email = ? AND sub.user_email = users.user_email"
          <> "RETURNING users.name, users.user_email, users.default_office, sub.is_admin"
      )
      (office, email)
  where
    sub =
      "SELECT u.user_email, offices.name AS office_name, CASE WHEN admins.user_email IS NULL THEN false ELSE true END AS is_admin"
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
    "UPDATE users LEFT JOIN admins ON users.user_email = admins.user_email SET access_token = NULL, refresh_token = NULL, expire_date = NULL WHERE user_email = ? RETURNING id_token"
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
          <> "client_name text"
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
      execute conn "INSERT INTO admins (user_email) VALUES (?) ON CONFLICT (user_email) DO NOTHING" (Only x)
    Nothing -> pure 0
  pure pool

exec :: (MonadIO m, MonadReader Env m, ToRow r) => Query -> r -> m ()
exec q r = do
  conns <- asks pool
  _ <- liftIO . withResource conns $ \conn -> execute conn q r
  pure ()

executeManyIO :: (ToRow r) => Pool Connection -> Query -> [r] -> IO ()
executeManyIO conns q rs = withResource conns $ \conn -> executeMany conn q rs $> ()

query'_ :: (MonadIO m, MonadReader Env m, FromRow r) => Query -> m [r]
query'_ q = do
  conns <- asks pool
  liftIO . withResource conns $ \conn -> query_ conn q

query' :: (MonadIO m, MonadReader Env m, ToRow x, FromRow r) => Query -> x -> m [r]
query' q x = do
  conns <- asks pool
  liftIO . withResource conns $ \conn -> query conn q x

queryIO :: (ToRow x, FromRow r) => Pool Connection -> Query -> x -> IO [r]
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
