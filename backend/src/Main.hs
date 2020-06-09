module Main where

import API (api, rootAPI)
import Control.Monad.Reader (runReaderT)
import Data.Env (Env (..))
import Data.String (fromString)
import Data.Text (Text, pack)
import Data.Yaml (decodeFileThrow)
import Database (initDatabase)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (hContentType, hOrigin)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, Middleware, requestHeaders, responseFile)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant.API ((:<|>) (..))
import Servant.Server (hoistServerWithContext, serveWithContext)
import Servant.Server.StaticFiles (serveDirectoryWith)
import Server (apiHandler, authServerContext, context, swaggerHandler)
import System.Environment (getArgs, getEnv)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings (..))

applyCors :: Middleware
applyCors = cors $ \req -> case map snd (filter ((==) hOrigin . fst) (requestHeaders req)) of
  [] -> Just simpleCorsResourcePolicy
  (x : _) -> Just simpleCorsResourcePolicy {corsOrigins = Just ([x], True)}

mkApp :: Manager -> Maybe Text -> Env -> Application
mkApp m email env =
  applyCors $ serveWithContext rootAPI (authServerContext m email) $
    ( swaggerHandler
        :<|> hoistServerWithContext api context (flip runReaderT env) apiHandler
        :<|> serveDirectoryWith ((defaultWebAppSettings frontendPath) {ss404Handler = Just serveIndex})
    )

serveIndex :: Application
serveIndex _ respond = respond $ responseFile status200 [(hContentType, "text/html")] (frontendPath <> "/index.html") Nothing

port :: Int
port = 8000

frontendPath :: String
frontendPath = "./static"

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
