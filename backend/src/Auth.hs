module Auth (contextProxy, mkAuthServerContext) where

import Control.Monad.Except ((<=<), ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Env (Env)
import Data.Functor ((<&>))
import Data.Proxy (Proxy (..))
import Data.Text (Text, intercalate)
import Data.Text.Encoding (encodeUtf8)
import Data.User (AdminUser (..), User (..))
import qualified Data.Vault.Lazy as V
import qualified Database as DB
import Network.HTTP.Types (hLocation, status302, status401)
import Network.Wai (Middleware, Request (pathInfo), mapResponseHeaders, requestHeaders, responseLBS, vault)
import OpenID (refreshAccessToken)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server (Context (..), Handler, err401, errBody)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Web.Cookie (parseCookiesText)

type instance AuthServerData (AuthProtect "fum-cookie") = User

type instance AuthServerData (AuthProtect "admin") = AdminUser

type ContextContent = '[AuthHandler Request User, AuthHandler Request AdminUser]

contextProxy :: Proxy ContextContent
contextProxy = Proxy

mkAuthServerContext :: (MonadReader Env m, MonadIO m) => m (Context ContextContent, Middleware)
mkAuthServerContext = do
  key <- liftIO V.newKey
  env <- ask
  pure (authHandler key :. adminAuthHandler key env :. EmptyContext, authMiddleware key env)

authHandler :: V.Key User -> AuthHandler Request User
authHandler key = mkAuthHandler $ login key

adminAuthHandler :: V.Key User -> Env -> AuthHandler Request AdminUser
adminAuthHandler key env = mkAuthHandler $ \req -> login key req >>= flip runReaderT env . DB.isAdmin >>= \case
  Just admin -> pure admin
  Nothing -> throwError $ err401 {errBody = "User is not an admin"}

login :: V.Key User -> Request -> Handler User
login key req = either throw401 pure (maybeToEither "No user data received from middleware" $ V.lookup key (vault req))
  where
    throw401 msg = throwError $ err401 {errBody = msg}

authMiddleware :: V.Key User -> Env -> Middleware
authMiddleware key env app req respond = case pathInfo req of
  ["login"] -> app req respond
  ["return"] -> app req respond
  path -> runReaderT (runExceptT (verifyLogin req)) env >>= \case
    Left e -> case path of
      "api" : _ -> respond $ responseLBS status401 [] $ pack e
      _ -> respond $ responseLBS status302 [(hLocation, "/login"), ("Set-Cookie", cookie path)] $ pack e
    Right (user, tokenCookie) ->
      let req' = req {vault = V.insert key user (vault req)}
       in app req' (\res -> let res' = maybe res (\c -> mapResponseHeaders (("Set-Cookie", c) :) res) tokenCookie in respond res')
  where
    cookie path = encodeUtf8 $ "prevUrl=/" <> intercalate "/" path <> ";HttpOnly;Secure;Path=/return"

verifyLogin :: (MonadReader Env m, MonadIO m) => Request -> ExceptT String m (User, Maybe ByteString)
verifyLogin req = getCookie req >>= DB.checkUser >>= \case
  Nothing -> throwError "accessToken not recognized"
  Just (Left token) -> refreshAccessToken token <&> second Just
  Just (Right user) -> pure (user, Nothing)

getCookie :: Monad m => Request -> ExceptT String m Text
getCookie =
  liftEither . maybeToEither "Missing token in cookie" . lookup "_Host-accessToken" . parseCookiesText
    <=< liftEither . maybeToEither "Missing cookie header" . lookup "cookie" . requestHeaders

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right
