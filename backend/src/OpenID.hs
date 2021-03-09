module OpenID (openidHandler, OpenIDAPI) where

import Control.Exception (throwIO)
import Control.Lens (preview)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Crypto.JWT (uri)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (Manager, Request (secure), httpLbs)
import Network.URI (parseURI, uriToString)
import OpenID.Connect.Client.Flow.AuthorizationCode (ClientSecret (AssignedSecretText), Credentials (..), Provider (providerDiscovery), RedirectTo (..), authenticationRedirect, defaultAuthenticationRequest, discoveryAndKeys, email, openid, profile)
import Servant.API hiding (URI)
import Servant.Server (err302, err400, err403, errBody, errHeaders)
import System.Environment (getEnv)
import Types (Server)
import Web.Cookie (parseCookies, renderSetCookie)

type OpenIDAPI =
  Login :<|> Success :<|> Failure

type Login = "login" :> Get '[PlainText] NoContent

type Success =
  "return"
    :> QueryParam "code" Text
    :> QueryParam "state" Text
    :> Header "cookie" SessionCookie
    :> Get '[PlainText] NoContent

type Failure =
  "return"
    :> QueryParam "error" Text
    :> QueryParam "state" Text
    :> Header "cookie" SessionCookie
    :> Get '[PlainText] NoContent

newtype SessionCookie = MkSessionCookie ByteString

openidHandler :: Manager -> Server OpenIDAPI
openidHandler m = login m :<|> success m :<|> failed

mkCredentials :: IO Credentials
mkCredentials = do
  clientID <- getEnv "OPENID_CLIENT_ID"
  secretText <- getEnv "OPENID_SECRET_TEXT"
  redirectUri <- getEnv "OPENID_REDIRECT_URI"
  pure $ Credentials (pack clientID) (AssignedSecretText $ pack secretText) (fromJust $ preview uri (fromString redirectUri))

mkProvider :: Manager -> IO Provider
mkProvider m = do
  Just configUri <- parseURI <$> getEnv "OPENID_CONFIG_URI"
  result <- discoveryAndKeys (\req -> req {secure = uriScheme configUri == "https:"} `httpLbs` m) configUri
  case result of
    Right (provider, _) -> pure provider
    Left err -> throwIO err

login :: Manager -> Server Login
login m = do
  creds <- liftIO mkCredentials
  provider <- liftIO $ mkProvider m
  let req = defaultAuthenticationRequest (openid <> email <> profile) creds
  r <- liftIO $ authenticationRedirect (providerDiscovery provider) req
  case r of
    Left e -> throwError (err403 {errBody = LChar8.pack (show e)})
    Right (RedirectTo redirectUri cookie) ->
      throwError
        ( err302
            { errHeaders =
                [ ("Location", Char8.pack (uriToString id redirectUri [])),
                  ("Set-Cookie", LChar8.toStrict (toLazyByteString (renderSetCookie (cookie "session"))))
                ]
            }
        )

success :: Manager -> Server Success
success = undefined

failed :: Server Failure
failed err _ _ = throwError $ err400 {errBody = maybe "authentication failure" (LChar8.fromStrict . encodeUtf8) err}

instance FromHttpApiData SessionCookie where
  parseUrlPiece = parseHeader . encodeUtf8
  parseHeader bs = case lookup "session" (parseCookies bs) of
    Nothing -> Left "session cookie missing"
    Just val -> Right (MkSessionCookie val)
