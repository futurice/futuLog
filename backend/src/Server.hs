module Server (apiHandler, swaggerHandler, authServerContext, context, Server) where

import API
import Auth (authServerContext, context)
import Control.Lens ((&), (.~), (?~))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ClientRequest (shiftName)
import Data.Config (Shift (name), shiftSite)
import Data.Env (Env (..))
import Data.Functor (($>))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Swagger (Scheme (Http, Https), info, schemes, title, version)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.User (User (..))
import Database (confirmWorkmode, getAllWorkmodes, getLastShiftsFor, getOfficeCapacityOn, queryWorkmode, saveShift)
import Logic (registerWorkmode)
import Servant.API ((:<|>) (..), (:>), NoContent (..))
import Servant.Server (Handler, ServerT, err400, errBody)
import qualified Servant.Server as S
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (swaggerSchemaUIServer)

type Server api = ServerT api (ReaderT Env Handler)

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
