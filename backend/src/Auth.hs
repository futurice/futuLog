module Auth (contextProxy, mkAuthServerContext, getAuth64, getCookie, getUsername) where

import Control.Monad.Except ((<=<), ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Text (Text, breakOn, isPrefixOf, replace)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Base64 (encodeBase64)
import Data.User (AdminUser (..), ContactUser (..), User (..))
import qualified Data.Vault.Lazy as V
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody)
import qualified Network.HTTP.Client as C
import Network.HTTP.Types (hAuthorization, status401)
import Network.Wai (Middleware, Request, requestHeaders, responseLBS, vault)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server (Context (..), Handler, err401, errBody)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import System.Environment (getEnv)
import Web.Cookie (parseCookiesText)

type instance AuthServerData (AuthProtect "fum-cookie") = User

type instance AuthServerData (AuthProtect "admin") = AdminUser

type ContextContent = '[AuthHandler Request User, AuthHandler Request AdminUser]

contextProxy :: Proxy ContextContent
contextProxy = Proxy

mkAuthServerContext :: Manager -> Maybe User -> [Text] -> IO (Context ContextContent, Middleware)
mkAuthServerContext m user admins = do
  key <- V.newKey
  pure (authHandler key :. adminAuthHandler key :. EmptyContext, authMiddleware user key admins m)

authHandler :: V.Key User -> AuthHandler Request User
authHandler key = mkAuthHandler $ login key

adminAuthHandler :: V.Key User -> AuthHandler Request AdminUser
adminAuthHandler key = mkAuthHandler $ \req -> do
  user <- login key req
  if isAdmin user
    then pure $ MkAdmin user
    else throwError $ err401 {errBody = "User is not an admin"}

login :: V.Key User -> Request -> Handler User
login key req = either throw401 pure (maybeToEither "No user data received from middleware" $ V.lookup key (vault req))
  where
    throw401 msg = throwError $ err401 {errBody = msg}

authMiddleware :: Maybe User -> V.Key User -> [Text] -> Manager -> Middleware
authMiddleware maybeUser key admins m app req respond = do
  eitherUser <- case maybeUser of
    Just x -> pure $ Right x
    _ -> runExceptT $ verifyLogin m req admins
  case eitherUser of
    Left e -> respond $ responseLBS status401 [] $ pack e
    Right user -> app (req {vault = V.insert key user (vault req)}) respond

verifyLogin :: Manager -> Request -> [Text] -> ExceptT String IO User
verifyLogin manager req admins = do
  cookie <- getCookie req
  username <- getUsername cookie
  auth <- getAuth64
  request <- liftIO $ parseRequest "https://prox.app.futurice.com/contacts/contacts.json"
  let request' = request {C.requestHeaders = C.requestHeaders request <> [(hAuthorization, "Basic " <> encodeUtf8 auth)]}
  response <- liftIO $ httpLbs request' manager
  case decode (responseBody response) of
    Just (MkContactUser a b email) -> withAvatar username $ \p -> MkUser a b email p p p (email `elem` admins)
    Nothing -> throwError "Failed to authorize to proxy"
  where
    withAvatar u f = do
      url <- liftIO $ T.pack <$> getEnv "SERVICE_URL"
      pure . f $ url <> "/api/avatar/" <> u

getUsername :: Text -> ExceptT String IO Text
getUsername c =
  if "uid%3D" `isPrefixOf` cookie'
    then
      let (name, _) = breakOn "%3B" $ T.drop 6 cookie'
       in pure name
    else throwError "No username in cookie"
  where
    cookie' = replace "\"" "" c

getCookie :: Request -> ExceptT String IO Text
getCookie =
  liftEither . maybeToEither "Missing token in cookie" . lookup "auth_pubtkt" . parseCookiesText
    <=< liftEither . maybeToEither "Missing cookie header" . lookup "cookie" . requestHeaders

getAuth64 :: MonadIO m => m Text
getAuth64 = do
  userName <- liftIO $ T.pack <$> getEnv "PROXY_USER"
  pass <- liftIO $ T.pack <$> getEnv "PROXY_PASS"
  pure . encodeBase64 $ userName <> ":" <> pass

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right
