module Database where

import Control.Monad (when)
import Control.Monad.Catch (Exception, MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Retry (RetryStatus (..), exponentialBackoff, recoverAll)
import Data.ByteString (ByteString)
import Data.ClientRequest (RegisterWorkmode (..))
import Data.Env (Env (..))
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, createPool, withResource)
import Data.Workmode (Workmode (..))
import Database.PostgreSQL.Simple (Connection, FromRow, Query, ToRow, close, connectPostgreSQL, execute, execute_, query_)

getAllWorkmodes :: (MonadIO m, MonadReader Env m) => m [RegisterWorkmode]
getAllWorkmodes = query'_ "SELECT * FROM workmodes"

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
          <> "shift_name text not null, "
          <> "assignment_date date not null, "
          <> "user_email text not null"
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

retry :: (MonadIO m, MonadMask m) => m a -> m a
retry x = recoverAll (exponentialBackoff 100) $ \RetryStatus {rsIterNumber} ->
  when
    (rsIterNumber > 0)
    (liftIO $ putStrLn "failed to connect to database, retrying")
    *> x
