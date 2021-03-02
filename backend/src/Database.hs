{-# LANGUAGE TupleSections #-}

module Database where

import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Retry (RetryStatus (..), exponentialBackoff, recoverAll)
import Data.ByteString (ByteString)
import Data.ClientRequest (AdminWorkmode (..), Capacity (..), Contact (..), RegisterWorkmode (..), SetShift (..), UserWorkmode (..), WorkmodeId (..))
import Data.Env (Env (..), ShiftAssignment (..), shiftAssignmentName, shiftAssignmentSite)
import Data.Maybe (listToMaybe)
import Data.Pool (Pool, createPool, withResource)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (utctDay), getCurrentTime)
import Data.User (User (..))
import Data.Workmode (Workmode (..))
import Database.PostgreSQL.Simple (Connection, FromRow, In (..), Only (..), Query, ToRow, close, connectPostgreSQL, execute, execute_, query, query_)
import System.Environment (lookupEnv)

getAllWorkmodes :: (MonadIO m, MonadReader Env m) => Text -> Day -> Day -> m [UserWorkmode]
getAllWorkmodes office start end =
  query'
    "SELECT * FROM workmodes WHERE workmode = 'Office' AND site = ? AND date >= ? AND date <= ? ORDER BY date ASC"
    (office, start, end)

queryWorkmode :: (MonadIO m, MonadReader Env m) => Text -> Day -> m (Maybe UserWorkmode)
queryWorkmode email day = listToMaybe <$> query' "SELECT * FROM workmodes WHERE user_email = ? AND date = ?" (email, day)

queryWorkmodes :: (MonadIO m, MonadReader Env m) => Text -> Day -> Day -> m [UserWorkmode]
queryWorkmodes email start end = query' "SELECT * FROM workmodes WHERE user_email = ? AND date >= ? AND date <= ?" (email, start, end)

queryContacts :: (MonadIO m, MonadReader Env m) => Text -> Day -> Day -> m [Contact]
queryContacts email start end =
  query'
    "SELECT site, date FROM workmodes WHERE workmode = 'Office' AND user_email = ? AND date >= ? AND date <= ? ORDER BY date DESC"
    (email, start, end)
    >>= mapM
      ( \t@(site, date) ->
          MkContact date site
            <$> query'
              ( "SELECT * FROM users WHERE user_email IN ("
                  <> "SELECT user_email FROM workmodes WHERE workmode = 'Office' AND site = ? AND date = ?"
                  <> ")"
              )
              t
      )

updateWorkmodes :: (MonadIO m, MonadReader Env m) => [AdminWorkmode] -> m ()
updateWorkmodes = mapM_ $ \(MkAdminWorkmode {email, site, date, workmode}) -> do
  users <- query' "SELECT * FROM users WHERE user_email = ?" (Only email)
  case users of
    [user] -> saveWorkmode user $ MkRegisterWorkmode site date workmode
    _ -> pure ()

deleteWorkmodes :: (MonadIO m, MonadReader Env m) => [WorkmodeId] -> m ()
deleteWorkmodes = mapM_ $ \(MkWorkmodeId date email) -> exec "DELETE FROM workmodes WHERE date = ? AND user_email = ?" (date, email)

confirmWorkmode :: (MonadIO m, MonadReader Env m) => Text -> Bool -> Day -> m ()
confirmWorkmode email status day = exec "UPDATE workmodes SET confirmed = ? WHERE user_email = ? AND date = ?" (status, email, day)

getLastShiftsFor :: (MonadIO m, MonadReader Env m) => Text -> m [ShiftAssignment]
getLastShiftsFor user =
  query'
    ( "(SELECT * FROM shift_assignments WHERE user_email = ? AND assignment_date > current_date - integer '14')"
        <> " UNION "
        <> "("
        <> shiftQuery
        <> ")"
        <> "ORDER BY assignment_date DESC"
    )
    (user, user)

shiftQuery :: Query
shiftQuery = "SELECT * FROM shift_assignments WHERE user_email = ? ORDER BY assignment_date DESC LIMIT 1"

getPeople :: (MonadIO m, MonadReader Env m) => m [User]
getPeople = query'_ "SELECT * FROM users"

-- TODO: Optimize
getOfficeBooked :: (MonadIO m, MonadReader Env m) => Text -> Day -> Day -> m [Capacity]
getOfficeBooked office start end =
  do
    days <-
      fmap fromOnly
        <$> query'
          "SELECT DISTINCT date FROM workmodes WHERE site = ? AND date >= ? AND date <= ? AND workmode = 'Office' ORDER BY date DESC"
          (office, start, end)
    mapM
      ( \day ->
          (day,) . fmap fromOnly
            <$> query' "SELECT DISTINCT user_email FROM workmodes WHERE site = ? AND date = ? AND workmode = 'Office'" (office, day)
      )
      days
    >>= mapM
      ( \(day, emails :: [Text]) ->
          MkCapacity day
            <$> query'
              "SELECT * FROM users WHERE user_email IN ?"
              (Only $ In emails)
      )

saveShift :: (MonadIO m, MonadReader Env m) => Text -> SetShift -> m ()
saveShift email MkSetShift {shiftName = name, site = office} = do
  lastShift <- query' shiftQuery (Only email)
  today <- liftIO $ utctDay <$> getCurrentTime
  case lastShift of
    [s]
      | shiftAssignmentName s == name && shiftAssignmentSite s == office -> pure ()
      | assignmentDate s == today ->
        exec
          "UPDATE shift_assignments SET user_email = ?, site = ?, shift_name = ? WHERE assignment_date = current_date"
          (email, office, name)
    _ ->
      exec
        "INSERT INTO shift_assignments (user_email, assignment_date, site, shift_name) VALUES (?, current_date, ?, ?)"
        (email, office, name)

saveWorkmode :: (MonadIO m, MonadReader Env m) => User -> RegisterWorkmode -> m ()
saveWorkmode user@(MkUser {email}) MkRegisterWorkmode {site, date, workmode} = do
  exec "DELETE FROM workmodes WHERE user_email = ? AND date = ?" (email, date)
  case workmode of
    Home -> mkSimpleQuery "Home"
    Leave -> mkSimpleQuery "Leave"
    (Client name) ->
      exec
        "INSERT INTO workmodes (user_email, site, date, workmode, client_name) VALUES (?, ?, ?, ?, ?)"
        (email, site, date, "Client" :: String, name)
    (Office confirmed) ->
      exec
        "INSERT INTO workmodes (user_email, site, date, workmode, confirmed) VALUES (?, ?, ?, ?, ?)"
        (email, site, date, "Office" :: String, confirmed)
  exec
    ( "INSERT INTO users (name, user_email, portrait_full_url, portrait_thumb_url, isAdmin) "
        <> "VALUES (?, ?, ?, ?, ?) ON CONFLICT (user_email) DO "
        <> "UPDATE SET name = EXCLUDED.name, portrait_full_url = EXCLUDED.portrait_full_url, "
        <> "portrait_thumb_url = EXCLUDED.portrait_thumb_url, isAdmin = EXCLUDED.isAdmin"
    )
    user
  where
    mkSimpleQuery s =
      exec
        "INSERT INTO workmodes (user_email, site, date, workmode) VALUES (?, ?, ?, ?)"
        (email, site, date, s :: String)

initDatabase :: MonadIO m => ByteString -> m (Pool Connection)
initDatabase connectionString = do
  pool <- liftIO $ createPool (retry $ connectPostgreSQL connectionString) close 2 60 10
  _ <- liftIO . withResource pool $ \conn ->
    execute_
      conn
      ( "CREATE TABLE IF NOT EXISTS workmodes ("
          <> "user_email text not null, "
          <> "site text not null, "
          <> "date date not null, "
          <> "workmode text not null, "
          <> "confirmed bool, "
          <> "client_name text"
          <> ")"
      )
  _ <- liftIO . withResource pool $ \conn ->
    execute_
      conn
      ( "CREATE TABLE IF NOT EXISTS shift_assignments ("
          <> "user_email text not null, "
          <> "assignment_date date not null, "
          <> "site text not null, "
          <> "shift_name text not null"
          <> ")"
      )
  _ <- liftIO . withResource pool $ \conn ->
    execute_
      conn
      ( "CREATE TABLE IF NOT EXISTS users ("
          <> "name text not null, "
          <> "user_email text PRIMARY KEY, "
          <> "portrait_full_url text not null, "
          <> "portrait_thumb_url text not null, "
          <> "isAdmin boolean not null"
          <> ")"
      )
  _ <- liftIO . withResource pool $ \conn -> execute_ conn "CREATE TABLE IF NOT EXISTS admins (user_email text PRIMARY KEY)"
  admin <- liftIO $ lookupEnv "INITAL_ADMIN"
  _ <- case admin of
    Just x -> liftIO . withResource pool $ \conn ->
      execute conn "INSERT INTO admins (user_email) VALUES (?) ON CONFLICT (user_email) DO NOTHING" (Only x)
    Nothing -> pure 0
  pure pool

exec :: (MonadIO m, MonadReader Env m, ToRow r) => Query -> r -> m ()
exec q r = do
  conns <- pool <$> ask
  _ <- liftIO . withResource conns $ \conn -> execute conn q r
  pure ()

query'_ :: (MonadIO m, MonadReader Env m, FromRow r) => Query -> m [r]
query'_ q = do
  conns <- pool <$> ask
  liftIO . withResource conns $ \conn -> query_ conn q

query' :: (MonadIO m, MonadReader Env m, ToRow x, FromRow r) => Query -> x -> m [r]
query' q x = do
  conns <- pool <$> ask
  liftIO . withResource conns $ \conn -> query conn q x

retry :: (MonadIO m, MonadMask m) => m a -> m a
retry x = recoverAll (exponentialBackoff 100) $ \RetryStatus {rsIterNumber} ->
  when
    (rsIterNumber > 0)
    (liftIO $ putStrLn "failed to connect to database, retrying")
    *> x
