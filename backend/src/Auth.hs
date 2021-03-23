module Auth (contextProxy, mkAuthServerContext, makeProxyRequest, getCookie, getUsername) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except ((<=<), ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
--import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import Data.Env (Env)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Text (Text, breakOn, isPrefixOf, replace)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Base64 (encodeBase64)
import Data.User (AdminUser (..), User (..))
import qualified Data.Vault.Lazy as V
import qualified Database as DB
import Network.HTTP.Client (Manager, httpLbs, parseRequest)
import qualified Network.HTTP.Client as C
import Network.HTTP.Types (hAuthorization, hLocation, status302)
import Network.Wai (Middleware, Request (pathInfo), requestHeaders, responseLBS, vault)
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

mkAuthServerContext :: Manager -> Env -> IO (Context ContextContent, Middleware)
mkAuthServerContext m env = do
  key <- V.newKey
  pure (authHandler key :. adminAuthHandler key :. EmptyContext, authMiddleware key m env)

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

authMiddleware :: V.Key User -> Manager -> Env -> Middleware
authMiddleware key m env app req respond = case pathInfo req of
  ["login"] -> app req respond
  ["return"] -> app req respond
  _ -> runExceptT (verifyLogin m env req) >>= \case
    Left e -> respond $ responseLBS status302 [(hLocation, "/login")] $ pack e
    Right user -> app (req {vault = V.insert key user (vault req)}) respond

verifyLogin :: Manager -> Env -> Request -> ExceptT String IO User
verifyLogin _ env req = getCookie req >>= DB.checkUser env >>= \case
  Nothing -> throwError "accessToken not recognized"
  Just (Left _token) -> undefined
  Just (Right user) -> pure user

{-username <- getUsername cookie
response <- makeProxyRequest manager "https://prox.app.futurice.com/contacts/contacts.json"
case decode (responseBody response) of
  Just users -> case filterUser username users of
    [MkContactUser _ name email thumb image] -> do
      let user = MkUser name email image thumb False
      admin <- DB.isAdmin env user
      pure $ user {isAdmin = isJust admin}
    _ -> throwError "User not found in contacts"
  Nothing -> throwError "Failed to authorize to proxy"
where
  filterUser u = filter (\(MkContactUser fumName _ _ _ _) -> fumName == u) -}

makeProxyRequest :: (MonadThrow m, MonadIO m) => Manager -> String -> m (C.Response BL.ByteString)
makeProxyRequest m url = do
  auth <- getAuth64
  request <- parseRequest url
  let request' = request {C.requestHeaders = C.requestHeaders request <> [(hAuthorization, "Basic " <> encodeUtf8 auth)]}
  liftIO $ httpLbs request' m

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
  liftEither . maybeToEither "Missing token in cookie" . lookup "_Host-accessToken" . parseCookiesText
    <=< liftEither . maybeToEither "Missing cookie header" . lookup "cookie" . requestHeaders

getAuth64 :: MonadIO m => m Text
getAuth64 = do
  userName <- liftIO $ T.pack <$> getEnv "PROXY_USER"
  pass <- liftIO $ T.pack <$> getEnv "PROXY_PASS"
  pure . encodeBase64 $ userName <> ":" <> pass

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right
