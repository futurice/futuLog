module Main where

import API (api)
import Control.Monad.Reader (runReaderT)
import Data.Env (Env (..))
import Data.String (fromString)
import Data.Yaml (decodeFileThrow)
import Database (initDatabase)
import Network.HTTP.Types.Header (hOrigin)
import Network.Wai (Application, Middleware, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant.Server (hoistServer, serve)
import Server (handler)
import System.Environment (getEnv)

applyCors :: Middleware
applyCors = cors $ \req -> case map snd (filter ((==) hOrigin . fst) (requestHeaders req)) of
  [] -> Just simpleCorsResourcePolicy
  (x : _) -> Just simpleCorsResourcePolicy {corsOrigins = Just ([x], True)}

mkApp :: Env -> Application
mkApp env = applyCors $ serve api (hoistServer api (flip runReaderT env) handler)

port :: Int
port = 3000

main :: IO ()
main = do
  offices <- decodeFileThrow "./offices.yaml"
  shifts <- decodeFileThrow "./shifts.yaml"
  pool <- initDatabase . fromString =<< getEnv "DB_CONNECTION"
  putStrLn $ "Running server on port " <> show port
  run port $ mkApp MkEnv {offices, shifts, pool}
