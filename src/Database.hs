module Database where

import API (RegisterWorkmode (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data (Env (..), Timeslot (..), Workmode (..))
import Data.ByteString (ByteString)
import Data.Pool (Pool, createPool, withResource)
import Database.PostgreSQL.Simple (Connection, FromRow, Query, ToRow, close, connectPostgreSQL, execute, execute_, query_)

connectionString :: ByteString
connectionString = "postgresql://postgres:example_password@db:5432/office"

initDatabase :: MonadIO m => m (Pool Connection)
initDatabase = do
  pool <- liftIO $ createPool (connectPostgreSQL connectionString) close 2 60 10
  liftIO . withResource pool $ \conn ->
    execute_
      conn
      ( "CREATE TABLE IF NOT EXISTS workmodes ("
          <> "userEmail text not null, "
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
saveWorkmode MkRegisterWorkmode {userEmail, site, date, workmode} = do
  case workmode of
    Home -> mkSimpleQuery "Home"
    Leave -> mkSimpleQuery "Leave"
    (Client name) ->
      exec
        "INSERT INTO workmodes (userEmail, site, date, workmode, clientName) VALUES (?, ?, ?, ?, ?)"
        (userEmail, site, date, "Client" :: String, name)
    (Office name MkTimeslot {startTime, endTime}) ->
      exec
        "INSERT INTO workmodes (userEmail, site, date, workmode, roomName, startTime, endTime) VALUES (?, ?, ?, ?, ?, ?, ?)"
        (userEmail, site, date, "Office" :: String, name, startTime, endTime)
  where
    mkSimpleQuery s = exec "INSERT INTO workmodes (userEmail, site, date, workmode) VALUES (?, ?, ?, ?)" (userEmail, site, date, s :: String)
