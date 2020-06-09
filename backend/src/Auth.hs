module Auth (contextProxy, mkAuthServerContext) where

import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text (breakOn, isPrefixOf, replace, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.User (AdminUser (..), User (..))
import qualified Data.Vault.Lazy as V
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody)
import qualified Network.HTTP.Client as C
import Network.HTTP.Types (hCookie, status401)
import Network.Wai (Middleware, Request, requestHeaders, responseLBS, vault)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server (Context (..), Handler, err401, errBody)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Web.Cookie (parseCookiesText)

type instance AuthServerData (AuthProtect "fum-cookie") = User

type instance AuthServerData (AuthProtect "admin") = AdminUser

type ContextContent = '[AuthHandler Request User, AuthHandler Request AdminUser]

contextProxy :: Proxy ContextContent
contextProxy = Proxy

mkAuthServerContext :: Manager -> Maybe User -> [Text] -> IO (Context ContextContent, Middleware)
mkAuthServerContext m user admins = do
  key <- V.newKey
  pure (authHandler key :. adminAuthHandler key admins :. EmptyContext, authMiddleware user key m)

authHandler :: V.Key User -> AuthHandler Request User
authHandler key = mkAuthHandler $ login key

adminAuthHandler :: V.Key User -> [Text] -> AuthHandler Request AdminUser
adminAuthHandler key admins = mkAuthHandler $ \req -> do
  user <- login key req
  if email user `elem` admins
    then pure $ MkAdmin user
    else throwError $ err401 {errBody = "User is not an admin"}

login :: V.Key User -> Request -> Handler User
login key req = either throw401 pure (maybeToEither "No user data received from middleware" $ V.lookup key (vault req))
  where
    throw401 msg = throwError $ err401 {errBody = msg}

authMiddleware :: Maybe User -> V.Key User -> Manager -> Middleware
authMiddleware maybeUser key m app req respond = do
  eitherUser <- case maybeUser of
    Just x -> pure $ Right x
    _ -> runExceptT $ verifyLogin m req
  case eitherUser of
    Left e -> respond $ responseLBS status401 [] $ pack e
    Right user -> app (req {vault = V.insert key user (vault req)}) respond

verifyLogin :: Manager -> Request -> ExceptT String IO User
verifyLogin manager req = do
  cookieRaw <- liftEither $ maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
  cookie <- liftEither $ maybeToEither "Missing token in cookie" $ lookup "auth_pubtkt" $ parseCookiesText cookieRaw
  username <- getUsername cookie
  request <- liftIO $ parseRequest $ "https://fum.futurice.com/fum/api/users/" <> unpack username
  let request' = request {C.requestHeaders = C.requestHeaders request <> [(hCookie, "auth_pubtkt=" <> encodeUtf8 cookie)]}
  response <- liftIO $ httpLbs request' manager
  case decode (responseBody response) of
    Just user -> pure user
    Nothing -> throwError "Token not accepted by FUM"
  where
    cookie' c = replace "\"" "" c
    getUsername c =
      if "uid%3D" `isPrefixOf` cookie' c
        then
          let (name, _) = breakOn "%3B" $ T.drop 6 $ cookie' c
           in pure name
        else throwError "No username in cookie"

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right
