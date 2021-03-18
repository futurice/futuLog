module OpenID (openidHandler, OpenIDAPI) where

import Control.Exception (throwIO)
import Control.Lens (preview)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Crypto.JWT (ClaimsSet, uri)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Network.HTTP.Client (Manager, Request (secure), httpLbs)
import Network.URI (parseURI, uriToString)
import OpenID.Connect.Client.Flow.AuthorizationCode (ClientSecret (AssignedSecretText), Credentials (..), HTTPS, Provider (providerDiscovery), RedirectTo (..), UserReturnFromRedirect (..), authenticationRedirect, authenticationSuccess, defaultAuthenticationRequest, discoveryAndKeys, email, openid, profile)
import OpenID.Connect.TokenResponse
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
    :> Get '[JSON] (TokenResponse ClaimsSet)

type Failure =
  "return"
    :> QueryParam "error" Text
    :> QueryParam "state" Text
    :> Header "cookie" SessionCookie
    :> Get '[JSON] (TokenResponse ClaimsSet)

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
  result <- discoveryAndKeys (https m) configUri
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
success m (Just code) (Just state) (Just (MkSessionCookie cookie)) = do
  let browser =
        UserReturnFromRedirect
          { afterRedirectCodeParam = encodeUtf8 code,
            afterRedirectStateParam = encodeUtf8 state,
            afterRedirectSessionCookie = cookie
          }
  now <- liftIO getCurrentTime
  provider <- liftIO $ mkProvider m
  creds <- liftIO mkCredentials
  r <- liftIO $ authenticationSuccess (https m) now provider creds browser
  case r of
    Left e -> throwError (err403 {errBody = LChar8.pack (show e)})
    Right token -> pure token
success _ _ _ _ = failed (Just "missing params") Nothing Nothing

failed :: Server Failure
failed err _ _ = throwError $ err400 {errBody = maybe "authentication failure" (LChar8.fromStrict . encodeUtf8) err}

https :: Manager -> HTTPS IO
https m req = do
  Just configUri <- parseURI <$> getEnv "OPENID_CONFIG_URI"
  req {secure = uriScheme configUri == "https:"} `httpLbs` m

instance FromHttpApiData SessionCookie where
  parseUrlPiece = parseHeader . encodeUtf8
  parseHeader bs = case lookup "session" (parseCookies bs) of
    Nothing -> Left "session cookie missing"
    Just val -> Right (MkSessionCookie val)
