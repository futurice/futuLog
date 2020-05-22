module Database where

import API (RegisterWorkmode (..))
import Control.Monad.Catch (Exception, MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Retry (exponentialBackoff, recoverAll)
import Data (Env (..), Timeslot (..), Workmode (..))
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, createPool, withResource)
import Database.PostgreSQL.Simple (Connection, FromRow, Query, ToRow, close, connectPostgreSQL, execute, execute_, query_)

connectionString :: ByteString
connectionString = "postgresql://postgres:example_password@db:5432/office"

retry :: (MonadIO m, MonadMask m) => m a -> m a
retry x = recoverAll (exponentialBackoff 100) $ const (liftIO (putStrLn "failed to connect to database, retrying") *> x)

initDatabase :: (MonadIO m, MonadMask m) => m (Pool Connection)
initDatabase = do
  pool <- liftIO $ createPool (retry $ connectPostgreSQL connectionString) close 2 60 10
  liftIO . withResource pool $ \conn ->
    execute_
      conn
      ( "CREATE TABLE IF NOT EXISTS workmodes ("
          <> "userEmail text not null, "
          <> "fullName text not null, "
          <> "site text not null, "
          <> "date date not null, "
          <> "workmode text not null, "
          <> "roomName text, "
          <> "startTime time, "
          <> "endTime time, "
          <> "clientName text"
          <> ")"
      )
  pure pool

exec :: (MonadIO m, MonadReader Env m, ToRow r) => Query -> r -> m ()
exec q r = do
  conns <- pool <$> ask
  liftIO . withResource conns $ \conn ->
    execute conn q r
  pure ()

query'_ :: (MonadIO m, MonadReader Env m, FromRow r) => Query -> m [r]
query'_ q = do
  conns <- pool <$> ask
  liftIO . withResource conns $ \conn -> query_ conn q

getAllWorkmodes :: (MonadIO m, MonadReader Env m) => m [RegisterWorkmode]
getAllWorkmodes = query'_ "SELECT * FROM workmodes"

saveWorkmode :: (MonadIO m, MonadReader Env m) => RegisterWorkmode -> m ()
saveWorkmode MkRegisterWorkmode {userEmail, fullName, site, date, workmode} = do
  case workmode of
    Home -> mkSimpleQuery "Home"
    Leave -> mkSimpleQuery "Leave"
    (Client name) ->
      exec
        "INSERT INTO workmodes (userEmail, fullName, site, date, workmode, clientName) VALUES (?, ?, ?, ?, ?, ?)"
        (userEmail, fullName, site, date, "Client" :: String, name)
    (Office name MkTimeslot {startTime, endTime}) ->
      exec
        "INSERT INTO workmodes (userEmail, fullName site, date, workmode, roomName, startTime, endTime) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (userEmail, fullName, site, date, "Office" :: String, name, startTime, endTime)
  where
    mkSimpleQuery s =
      exec
        "INSERT INTO workmodes (userEmail, fullName, site, date, workmode) VALUES (?, ?, ?, ?, ?)"
        (userEmail, fullName, site, date, s :: String)
