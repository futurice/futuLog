module Database where

import Control.Monad (when)
import Control.Monad.Catch (Exception, MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Retry (RetryStatus (..), exponentialBackoff, recoverAll)
import Data.ByteString (ByteString)
import Data.ClientRequest (RegisterWorkmode (..), SetShift (..))
import Data.Env (Env (..), ShiftAssignment, shiftAssignmentName)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, createPool, withResource)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Workmode (Workmode (..))
import Database.PostgreSQL.Simple (Connection, FromRow, Only (..), Query, ToRow, close, connectPostgreSQL, execute, execute_, query, query_)

getAllWorkmodes :: (MonadIO m, MonadReader Env m) => m [RegisterWorkmode]
getAllWorkmodes = query'_ "SELECT * FROM workmodes"

getLastShiftsFor :: (MonadIO m, MonadReader Env m) => Text -> m [ShiftAssignment]
getLastShiftsFor user =
  query'
    ( "(SELECT * FROM shift_assignments WHERE user_email = ? AND assignment_date > current_date - integer '14')"
        <> " UNION "
        <> "("
        <> lastShiftQuery
        <> ")"
    )
    (user, user)

getOfficeCapacityOn :: (MonadIO m, MonadReader Env m) => Text -> Day -> m Int
getOfficeCapacityOn office day = fromOnly . head <$> query' "SELECT COUNT(*) FROM workmodes WHERE date = ? AND site = ?" (day, office)

saveShift :: (MonadIO m, MonadReader Env m) => SetShift -> m ()
saveShift MkSetShift {userEmail, shiftName = name} = do
  lastShift <- head <$> query' lastShiftQuery (Only userEmail)
  if shiftAssignmentName lastShift == name
    then pure ()
    else exec "INSERT INTO shift_assignments (user_email, assignment_date, shift_name) VALUES (?, current_date, ?)" (userEmail, name)

saveWorkmode :: (MonadIO m, MonadReader Env m) => RegisterWorkmode -> m ()
saveWorkmode MkRegisterWorkmode {userEmail, site, date, workmode} = do
  case workmode of
    Home -> mkSimpleQuery "Home"
    Leave -> mkSimpleQuery "Leave"
    (Client name) ->
      exec
        "INSERT INTO workmodes (user_email, site, date, workmode, client_name) VALUES (?, ?, ?, ?, ?)"
        (userEmail, site, date, "Client" :: String, name)
    (Office confirmed) ->
      exec
        "INSERT INTO workmodes (user_email, site, date, workmode, confirmed) VALUES (?, ?, ?, ?, ?)"
        (userEmail, site, date, "Office" :: String, confirmed)
  where
    mkSimpleQuery s =
      exec
        "INSERT INTO workmodes (user_email, site, date, workmode) VALUES (?, ?, ?, ?)"
        (userEmail, site, date, s :: String)

lastShiftQuery :: Query
lastShiftQuery = "SELECT * FROM shift_assignments WHERE user_email = ? AND assignment_date <= current_date - integer '14' ORDER BY assignment_date DESC LIMIT 1"

initDatabase :: (MonadIO m, MonadMask m) => ByteString -> m (Pool Connection)
initDatabase pass = do
  pool <- liftIO $ createPool (retry . connectPostgreSQL $ mkConnectionString pass) close 2 60 10
  liftIO . withResource pool $ \conn ->
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
  liftIO . withResource pool $ \conn ->
    execute_
      conn
      ( "CREATE TABLE IF NOT EXISTS shift_assignments ("
          <> "user_email text not null, "
          <> "assignment_date date not null, "
          <> "shift_name text not null"
          <> ")"
      )
  pure pool

mkConnectionString :: ByteString -> ByteString
mkConnectionString pass = "postgresql://postgres:" <> pass <> "@db:5432/office"

exec :: (MonadIO m, MonadReader Env m, ToRow r) => Query -> r -> m ()
exec q r = do
  conns <- pool <$> ask
  liftIO . withResource conns $ \conn -> execute conn q r
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
