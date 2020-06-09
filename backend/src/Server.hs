module Server (apiHandler, swaggerHandler, authServerContext, context, Server) where

import API
import Control.Lens ((&), (.~), (?~))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ClientRequest (shiftName)
import Data.Config (Shift (name), shiftSite)
import Data.Env (Env (..))
import Data.Functor (($>))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Swagger (Scheme (Http, Https), info, schemes, title, version)
import qualified Data.Text as T
import Data.Text (Text, breakOn, isPrefixOf, replace, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.User (User (..))
import Database (confirmWorkmode, getAllWorkmodes, getLastShiftsFor, getOfficeCapacityOn, queryWorkmode, saveShift)
import Logic (registerWorkmode)
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody)
import qualified Network.HTTP.Client as C
import Network.HTTP.Types (hCookie)
import Network.Wai (Request, requestHeaders)
import Servant.API ((:<|>) (..), (:>), NoContent (..))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server (Context (..), Handler, ServerT, err400, err401, errBody)
import qualified Servant.Server as S
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (swaggerSchemaUIServer)
import Web.Cookie (parseCookiesText)

type Server api = ServerT api (ReaderT Env Handler)

type instance AuthServerData (AuthProtect "fum-cookie") = User

swaggerHandler :: S.Server SwaggerAPI
swaggerHandler = swaggerSchemaUIServer swagger
  where
    swagger =
      toSwagger (Proxy :: Proxy ("api" :> API))
        & schemes ?~ [Https, Http]
        & info . title .~ "Office Tracker API"
        & info . version .~ "1.0"

apiHandler :: Server ProtectedAPI
apiHandler user = workmodeHandler user :<|> shiftHandler user :<|> officeHandler :<|> pure user

workmodeHandler :: User -> Server WorkmodeAPI
workmodeHandler MkUser {email} = regWorkmode :<|> confWorkmode :<|> queryWorkmode email :<|> getAllWorkmodes
  where
    regWorkmode m = registerWorkmode email m >>= \case
      Right _ -> pure NoContent
      Left err -> throwError $ err400 {errBody = pack err}
    confWorkmode day status = do
      today <- liftIO $ utctDay <$> getCurrentTime
      confirmWorkmode email (fromMaybe today day) status
      pure NoContent

shiftHandler :: User -> Server ShiftAPI
shiftHandler MkUser {email} = getShift :<|> (\office -> setShift office :<|> getShifts office)
  where
    getShift = listToMaybe <$> getLastShiftsFor email
    getShifts office = filter ((==) office . shiftSite) . shifts <$> ask
    setShift office x = do
      shiftNames <- fmap name <$> getShifts office
      if shiftName x `elem` shiftNames
        then saveShift email office x $> NoContent
        else throwError $ err400 {errBody = "specified shift does not exist"}

officeHandler :: Server OfficeAPI
officeHandler = getOffices :<|> getOfficeCapacityOn
  where
    getOffices = offices <$> ask

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
