module Main where

import API (api, rootAPI)
import Control.Monad.Reader (runReaderT)
import Data.Char (isSpace)
import Data.Env (Env (..))
import Data.String (fromString)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileThrow, decodeThrow)
import Database (initDatabase)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (hContentType, hOrigin)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, Middleware, requestHeaders, responseFile)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import OpenID (openidHandler)
import Servant.API ((:<|>) (..))
import Servant.Server (hoistServerWithContext, serveWithContext)
import Servant.Server.StaticFiles (serveDirectoryWith)
import Server (apiHandler, contextProxy, mkAuthServerContext, swaggerHandler)
import System.Environment (getEnv)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings (..))

applyCors :: Middleware
applyCors = cors $ \req -> case map snd (filter ((==) hOrigin . fst) (requestHeaders req)) of
  [] -> Just simpleCorsResourcePolicy
  (x : _) -> Just simpleCorsResourcePolicy {corsOrigins = Just ([x], True)}

mkApp :: Manager -> Env -> IO Application
mkApp m env = do
  (context, middleware) <- mkAuthServerContext m env
  pure $ applyCors $ middleware $
    serveWithContext
      rootAPI
      context
      ( swaggerHandler
          :<|> hoistServerWithContext api contextProxy (flip runReaderT env) (openidHandler m :<|> apiHandler m)
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
  offices <- decodeFileThrow "./offices.yaml"
  shiftsFile <- readFile "./shifts.yaml"
  shifts <-
    if all isSpace shiftsFile
      then pure []
      else decodeThrow . encodeUtf8 $ pack shiftsFile
  pool <- initDatabase . fromString =<< getEnv "DB_URL"
  manager <- newTlsManager
  putStrLn $ "Running server on port " <> show port
  app <- mkApp manager MkEnv {offices, shifts, pool}
  run port app
