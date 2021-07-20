module Main where

import API (api, rootAPI)
import Control.Exception (throwIO)
import Control.Monad.Reader (runReaderT)
import Data.Env (Env (..))
import Data.String (fromString)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Database (initDatabase, retry)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (hContentType, hOrigin)
import Network.HTTP.Types.Status (status200)
import Network.URI (parseURI)
import Network.Wai (Application, Middleware, requestHeaders, responseFile)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import OpenID (httpsDebug, openidHandler)
import OpenID.Connect.Client.Provider (Provider, discoveryAndKeys)
import Servant.API ((:<|>) (..))
import Servant.Server (hoistServerWithContext, serveWithContext)
import Servant.Server.StaticFiles (serveDirectoryWith)
import Server (apiHandler, contextProxy, mkAuthServerContext, swaggerHandler)
import System.Environment (getEnv)
import System.IO (hFlush, stdout)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings (..))

applyCors :: Middleware
applyCors = cors $ \req -> case map snd (filter ((==) hOrigin . fst) (requestHeaders req)) of
  [] -> Just simpleCorsResourcePolicy
  (x : _) -> Just simpleCorsResourcePolicy {corsOrigins = Just ([x], True)}

mkApp :: Env -> IO Application
mkApp env = do
  (context, middleware) <- mkAuthServerContext env
  pure $
    applyCors $
      middleware $
        serveWithContext
          rootAPI
          context
          ( swaggerHandler
              :<|> hoistServerWithContext api contextProxy (flip runReaderT env) (openidHandler :<|> apiHandler)
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
  time <- getCurrentTime
  put $ "---- Restart: " <> iso8601Show time <> " ----"
  put "Initializing database"
  pool <- initDatabase . fromString =<< getEnv "DB_URL"
  manager <- newTlsManager
  provider <- mkProvider manager
  app <- mkApp MkEnv {offices = [], pool, manager, provider}
  put $ "Running server on port " <> show port
  run port app

mkProvider :: Manager -> IO Provider
mkProvider m = do
  Just configUri <- parseURI <$> getEnv "OPENID_CONFIG_URI"
  result <- retry "identity provider" $ discoveryAndKeys (httpsDebug m) configUri
  case result of
    Right (provider, _) -> pure provider
    Left err -> throwIO err

put :: String -> IO ()
put s = putStrLn s *> hFlush stdout
