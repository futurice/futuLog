module Database where

import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Retry (RetryStatus (..), exponentialBackoff, recoverAll)
import Data.ByteString (ByteString)
import Data.ClientRequest (RegisterWorkmode (..), SetShift (..), UserWorkmode (..))
import Data.Env (Env (..), ShiftAssignment (..), shiftAssignmentName)
import Data.Maybe (listToMaybe)
import Data.Pool (Pool, createPool, withResource)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (utctDay), getCurrentTime)
import Data.Workmode (Workmode (..))
import Database.PostgreSQL.Simple (Connection, FromRow, Only (..), Query, ToRow, close, connectPostgreSQL, execute, execute_, query, query_)

getAllWorkmodes :: (MonadIO m, MonadReader Env m) => Text -> Day -> m [Text]
getAllWorkmodes office day =
  fmap fromOnly
    <$> query' "SELECT user_email FROM workmodes WHERE workmode = 'Office' AND site = ? AND date = ?" (office, day)

queryWorkmode :: (MonadIO m, MonadReader Env m) => Text -> Day -> m (Maybe UserWorkmode)
queryWorkmode email day = listToMaybe <$> query' "SELECT * FROM workmodes WHERE user_email = ? AND date = ?" (email, day)

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

getOfficeCapacityOn :: (MonadIO m, MonadReader Env m) => Text -> Day -> m Int
getOfficeCapacityOn office day = fromOnly . head <$> query' "SELECT COUNT(*) FROM workmodes WHERE date = ? AND site = ?" (day, office)

saveShift :: (MonadIO m, MonadReader Env m) => Text -> Text -> SetShift -> m ()
saveShift email office MkSetShift {shiftName = name} = do
  lastShift <-
    query'
      shiftQuery
      (Only email)
  today <- liftIO $ utctDay <$> getCurrentTime
  case lastShift of
    [s]
      | shiftAssignmentName s == name -> pure ()
      | assignmentDate s == today ->
        exec
          "UPDATE shift_assignments SET user_email = ?, site = ?, shift_name = ? WHERE assignment_date = current_date"
          (email, office, name)
    _ ->
      exec
        "INSERT INTO shift_assignments (user_email, assignment_date, site, shift_name) VALUES (?, current_date, ?, ?)"
        (email, office, name)

saveWorkmode :: (MonadIO m, MonadReader Env m) => Text -> RegisterWorkmode -> m ()
saveWorkmode email MkRegisterWorkmode {site, date, workmode} = do
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
