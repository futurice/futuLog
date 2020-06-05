module Server (apiHandler, swaggerHandler, authServerContext, context, Server) where

import API
import Control.Lens ((&), (.~), (?~))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ClientRequest (shiftName)
import Data.Config (Shift (name), shiftSite)
import Data.Env (Env (..))
import Data.Functor (($>))
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Swagger (Scheme (Http, Https), info, schemes, title, version)
import qualified Data.Text as T
import Data.Text (Text, breakOn, isPrefixOf, replace, unpack)
import Data.Text.Encoding (encodeUtf8)
import Database (getAllWorkmodes, getLastShiftsFor, getOfficeCapacityOn, queryWorkmode, saveShift)
import GHC.Generics (Generic)
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

type instance AuthServerData (AuthProtect "fum-cookie") = Text

swaggerHandler :: S.Server SwaggerAPI
swaggerHandler = swaggerSchemaUIServer swagger
  where
    swagger =
      toSwagger (Proxy :: Proxy ("api" :> API))
        & schemes ?~ [Https, Http]
        & info . title .~ "Office Tracker API"
        & info . version .~ "1.0"

apiHandler :: Server ProtectedAPI
apiHandler userEmail = workmodeHandler userEmail :<|> shiftHandler userEmail :<|> officeHandler

workmodeHandler :: Text -> Server WorkmodeAPI
workmodeHandler email = regWorkmode :<|> queryWorkmode email :<|> getAllWorkmodes
  where
    regWorkmode m = registerWorkmode email m >>= \case
      Right _ -> pure NoContent
      Left err -> throwError $ err400 {errBody = pack err}

shiftHandler :: Text -> Server ShiftAPI
shiftHandler email = getShift :<|> (\office -> setShift office :<|> getShifts office)
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

context :: Proxy '[AuthHandler Request Text]
context = Proxy

authServerContext :: Manager -> Maybe Text -> Context '[AuthHandler Request Text]
authServerContext m email = authHandler m email :. EmptyContext

authHandler :: Manager -> Maybe Text -> AuthHandler Request Text
authHandler m email = mkAuthHandler checkDev
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 {errBody = msg}
    checkDev req = maybe (handleLogin req) pure email
    handleLogin req = either throw401 (verifyLogin m) $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ lookup "auth_pubtkt" $ parseCookiesText cookie

verifyLogin :: Manager -> Text -> Handler Text
verifyLogin manager cookie = do
  user <- getUsername
  request <- liftIO $ parseRequest $ "https://fum.futurice.com/fum/api/users/" <> unpack user
  let request' = request {C.requestHeaders = C.requestHeaders request <> [(hCookie, "auth_pubtkt=" <> encodeUtf8 cookie)]}
  response <- liftIO $ httpLbs request' manager
  case decode (responseBody response) of
    Just (MkFumResponse {email}) -> pure email
    Nothing -> throwError $ err401 {errBody = "Token not accepted by FUM"}
  where
    cookie' = replace "\"" "" cookie
    getUsername =
      if "uid%3D" `isPrefixOf` cookie'
        then
          let (name, _) = breakOn "%3B" $ T.drop 6 cookie'
           in pure name
        else throwError $ err401 {errBody = "No username in cookie"}

data FumResponse
  = MkFumResponse
      { first_name :: Text,
        last_name :: Text,
        username :: Text,
        email :: Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)
