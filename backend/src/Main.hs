module Main where

import API (api)
import Control.Monad.Reader (runReaderT)
import Data.Env (Env (..))
import Data.String (fromString)
import Data.Text (Text, pack)
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
import System.Environment (getArgs, getEnv)

applyCors :: Middleware
applyCors = cors $ \req -> case map snd (filter ((==) hOrigin . fst) (requestHeaders req)) of
  [] -> Just simpleCorsResourcePolicy
  (x : _) -> Just simpleCorsResourcePolicy {corsOrigins = Just ([x], True)}

mkApp :: Manager -> Maybe Text -> Env -> Application
mkApp m email env =
  applyCors $ serveWithContext api (authServerContext m email) $
    hoistServerWithContext api context (flip runReaderT env) handler

port :: Int
port = 8000

main :: IO ()
main = do
  args <- getArgs
  let devEmail = case args of
        "--devEmail" : x : _ -> Just x
        _ -> Nothing
  offices <- decodeFileThrow "./offices.yaml"
  shifts <- decodeFileThrow "./shifts.yaml"
  pool <- initDatabase . fromString =<< getEnv "DB_URL"
  manager <- newTlsManager
  case devEmail of
    Nothing -> putStrLn $ "Running server on port " <> show port
    Just email -> putStrLn $ "Running development server on port " <> show port <> " with logged in email " <> email
  run port $ mkApp manager (pack <$> devEmail) MkEnv {offices, shifts, pool}
