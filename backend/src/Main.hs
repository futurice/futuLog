module Main where

import API (API)
import Control.Monad.Reader (runReaderT)
import Data.Env (Env (..))
import Data.Proxy (Proxy (..))
import Data.Yaml (decodeFileThrow)
import Database (initDatabase)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:>))
import Servant.Server (hoistServer, serve)
import Server (handler)

api :: Proxy ("api" :> API)
api = Proxy

mkApp :: Env -> Application
mkApp env = serve api (hoistServer api (flip runReaderT env) handler)

port :: Int
port = 3000

main :: IO ()
main = do
  offices <- decodeFileThrow "./offices.yaml"
  shifts <- decodeFileThrow "./shifts.yaml"
  pool <- initDatabase "example_password"
  putStrLn $ "Running server on port " <> show port
  run port $ mkApp MkEnv {offices, shifts, pool}
