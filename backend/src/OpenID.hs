module OpenID (openidHandler, https, refreshAccessToken, OpenIDAPI) where

import Control.Lens (firstOf, preview)
import Control.Lens.At (at)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Crypto.JWT (ClaimsSet, unregisteredClaims, uri)
import Data.Aeson.Lens (_String)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.Env (Env, manager, provider)
import Data.Functor (($>))
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Base64 (encodeBase64)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.User (OpenIdUser (..), User)
import qualified Database as DB
import Network.HTTP.Client (Manager, Request (secure), RequestBody (RequestBodyBS), Response (responseBody, responseStatus), httpLbs, method, requestBody, requestFromURI, requestHeaders)
import Network.HTTP.Types (hAuthorization, hContentType, hLocation, statusIsSuccessful)
import Network.URI (parseURI, uriToString)
import OpenID.Connect.Client.Flow.AuthorizationCode (ClientSecret (AssignedSecretText), Credentials (..), Discovery (tokenEndpoint), HTTPS, Provider (providerDiscovery), RedirectTo (..), UserReturnFromRedirect (..), authenticationRedirect, authenticationSuccess, defaultAuthenticationRequest, email, openid, profile)
import OpenID.Connect.Client.Provider (URI (..))
import OpenID.Connect.TokenResponse
import Servant.API hiding (URI)
import Servant.Server (err302, err400, err403, errBody, errHeaders)
import System.Environment (getEnv)
import Types (Server)
import Web.Cookie (defaultSetCookie, parseCookies, renderSetCookie, sameSiteStrict, setCookieHttpOnly, setCookieName, setCookiePath, setCookieSameSite, setCookieSecure, setCookieValue)

type OpenIDAPI =
  Login :<|> Success :<|> Failure

type Login = "login" :> Get '[PlainText] NoContent

type Success =
  "return"
    :> QueryParam "code" Text
    :> QueryParam "state" Text
    :> Header "cookie" SessionCookie
    :> Get '[JSON] NoContent

type Failure =
  "return"
    :> QueryParam "error" Text
    :> QueryParam "state" Text
    :> Header "cookie" SessionCookie
    :> Get '[JSON] NoContent

data SessionCookie
  = MkSessionCookie
      { session :: ByteString,
        prevUrl :: Maybe ByteString
      }

openidHandler :: Server OpenIDAPI
openidHandler = login :<|> success :<|> failed

getClientSecret :: IO (Text, Text)
getClientSecret = do
  clientID <- getEnv "OPENID_CLIENT_ID"
  secretText <- getEnv "OPENID_SECRET_TEXT"
  pure (pack clientID, pack secretText)

mkCredentials :: IO Credentials
mkCredentials = do
  (clientID, secretText) <- getClientSecret
  redirectUri <- getEnv "OPENID_REDIRECT_URI"
  pure $ Credentials clientID (AssignedSecretText secretText) (fromJust $ preview uri (fromString redirectUri))

refreshAccessToken :: (MonadReader Env m, MonadIO m) => Text -> ExceptT String m User
refreshAccessToken token = asks (tokenEndpoint . providerDiscovery . provider) >>= \case
  Nothing -> throwError "Provider does not specify a token endpoint"
  Just (URI url) -> do
    m <- asks manager
    request <- liftIO $ requestFromURI url
    (clientID, secret) <- liftIO getClientSecret
    let body = "grant_type=refresh_token&refresh_token=" <> encodeUtf8 token
        auth = "Basic " <> encodeBase64 (clientID <> ":" <> secret)
        req' =
          request
            { method = "POST",
              requestBody = RequestBodyBS body,
              requestHeaders =
                [ (hAuthorization, encodeUtf8 auth),
                  (hContentType, "application/x-www-form-urlencoded")
                ]
            }
    response <- liftIO $ httpLbs req' m
    if not (statusIsSuccessful $ responseStatus response)
      then throwError . show $ responseBody response
      else undefined

login :: Server Login
login = do
  creds <- liftIO mkCredentials
  provider <- asks provider
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

saveUser :: (MonadIO m, MonadReader Env m) => UTCTime -> TokenResponse ClaimsSet -> m (Either BL.ByteString ())
saveUser now token = do
  let get key = firstOf (unregisteredClaims . at key . traverse . _String) (idToken token)
      expireDate = addUTCTime (fromIntegral . fromMaybe 0 $ expiresIn token) now
  case (get "name", get "email") of
    (Just n, Just e) -> do
      let user = MkOpenIdUser n e (fromMaybe "/default_picture.png" $ get "picture") expireDate (accessToken token) (refreshToken token)
      DB.saveUser user $> Right ()
    _ -> pure $ Left "email and/or name not part of the id token"

getCookie :: Text -> ByteString
getCookie token =
  BL.toStrict . toLazyByteString . renderSetCookie $
    defaultSetCookie
      { setCookieSecure = True,
        setCookieName = "_Host-accessToken",
        setCookieValue = encodeUtf8 token,
        setCookieHttpOnly = True,
        setCookieSameSite = Just sameSiteStrict,
        setCookiePath = Just "/"
      }

success :: Server Success
success (Just code) (Just state) (Just MkSessionCookie {session, prevUrl}) = do
  let browser =
        UserReturnFromRedirect
          { afterRedirectCodeParam = encodeUtf8 code,
            afterRedirectStateParam = encodeUtf8 state,
            afterRedirectSessionCookie = session
          }
  now <- liftIO getCurrentTime
  m <- asks manager
  provider <- asks provider
  creds <- liftIO mkCredentials
  r <- liftIO $ authenticationSuccess (https m) now provider creds browser
  case r of
    Left e -> throwError (err403 {errBody = LChar8.pack (show e)})
    Right token -> saveUser now token >>= \case
      Left err -> throwError err403 {errBody = err}
      Right () ->
        throwError
          err302
            { errHeaders =
                [ ("Set-Cookie", getCookie (accessToken token)),
                  ("Set-Cookie", "prevUrl=\"\"; Path=/return; Secure; HttpOnly; Expires=Thu Jan 01 1970 00:00:00 GMT"),
                  (hLocation, fromMaybe "/" prevUrl)
                ]
            }
success _ _ _ = failed (Just "missing params") Nothing Nothing

failed :: Server Failure
failed err _ _ = throwError $ err400 {errBody = maybe "authentication failure" (LChar8.fromStrict . encodeUtf8) err}

https :: Manager -> HTTPS IO
https m req = do
  Just configUri <- parseURI <$> getEnv "OPENID_CONFIG_URI"
  req {secure = uriScheme configUri == "https:"} `httpLbs` m

instance FromHttpApiData SessionCookie where
  parseUrlPiece = parseHeader . encodeUtf8
  parseHeader bs =
    let c = parseCookies bs
     in case lookup "session" c of
          Nothing -> Left "session cookie missing"
          Just val -> Right (MkSessionCookie val $ lookup "prevUrl" c)
