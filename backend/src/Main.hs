module Main where

import API (api)
import Control.Monad.Reader (runReaderT)
import Data.Env (Env (..))
import Data.String (fromString)
import Data.Yaml (decodeFileThrow)
import Database (initDatabase)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (hOrigin)
import Network.Wai (Application, Middleware, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant.Server (hoistServerWithContext, serveWithContext)
import Server (authServerContext, context, handler)
import System.Environment (getEnv)

applyCors :: Middleware
applyCors = cors $ \req -> case map snd (filter ((==) hOrigin . fst) (requestHeaders req)) of
  [] -> Just simpleCorsResourcePolicy
  (x : _) -> Just simpleCorsResourcePolicy {corsOrigins = Just ([x], True)}

mkApp :: Manager -> Env -> Application
mkApp m env = applyCors $ serveWithContext api (authServerContext m) $ hoistServerWithContext api context (flip runReaderT env) handler

port :: Int
port = 8000

main :: IO ()
main = do
  offices <- decodeFileThrow "./offices.yaml"
  shifts <- decodeFileThrow "./shifts.yaml"
  pool <- initDatabase . fromString =<< getEnv "DB_URL"
  manager <- newTlsManager
  putStrLn $ "Running server on port " <> show port
  run port $ mkApp manager MkEnv {offices, shifts, pool}
