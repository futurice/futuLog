module Auth (context, authServerContext) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text (breakOn, isPrefixOf, replace, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.User (User (..))
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody)
import qualified Network.HTTP.Client as C
import Network.HTTP.Types (hCookie)
import Network.Wai (Request, requestHeaders)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server (Context (..), Handler, err401, errBody)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Web.Cookie (parseCookiesText)

type instance AuthServerData (AuthProtect "fum-cookie") = User

context :: Proxy '[AuthHandler Request User]
context = Proxy

authServerContext :: Manager -> Maybe User -> Context '[AuthHandler Request User]
authServerContext m user = authHandler m user :. EmptyContext

authHandler :: Manager -> Maybe User -> AuthHandler Request User
authHandler m user = mkAuthHandler checkDev
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 {errBody = msg}
    checkDev req = maybe (handleLogin req) pure user
    handleLogin req = either throw401 (verifyLogin m) $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ lookup "auth_pubtkt" $ parseCookiesText cookie

verifyLogin :: Manager -> Text -> Handler User
verifyLogin manager cookie = do
  username <- getUsername
  request <- liftIO $ parseRequest $ "https://fum.futurice.com/fum/api/users/" <> unpack username
  let request' = request {C.requestHeaders = C.requestHeaders request <> [(hCookie, "auth_pubtkt=" <> encodeUtf8 cookie)]}
  response <- liftIO $ httpLbs request' manager
  case decode (responseBody response) of
    Just user -> pure user
    Nothing -> throwError $ err401 {errBody = "Token not accepted by FUM"}
  where
    cookie' = replace "\"" "" cookie
    getUsername =
      if "uid%3D" `isPrefixOf` cookie'
        then
          let (name, _) = breakOn "%3B" $ T.drop 6 cookie'
           in pure name
        else throwError $ err401 {errBody = "No username in cookie"}
