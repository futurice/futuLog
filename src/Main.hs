module Main where

import API (OfficeAPI)
import Control.Monad.Reader (runReaderT)
import Data (Env (..))
import Data.Proxy (Proxy (..))
import Data.Yaml (decodeFileThrow)
import Database (initDatabase)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant.Server (hoistServer, serve)
import Server (officeHandler)

api :: Proxy OfficeAPI
api = Proxy

mkApp :: Env -> Application
mkApp env = serve api (hoistServer api (flip runReaderT env) officeHandler)

port :: Int
port = 3000

main :: IO ()
main = do
  rooms <- decodeFileThrow "./rooms.yaml"
  pool <- initDatabase
  putStrLn $ "Running server on port " <> show port
  run port $ mkApp MkEnv {rooms, pool}
