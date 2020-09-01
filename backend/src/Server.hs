module Server (apiHandler, swaggerHandler, mkAuthServerContext, contextProxy, Server) where

import API
import Auth (contextProxy, mkAuthServerContext)
import qualified CSV
import Control.Lens ((&), (.~), (?~))
import Control.Monad ((<=<))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ClientRequest (SetShift (..), shiftName)
import Data.Config (Shift (name), shiftSite)
import Data.Env (Env (..))
import Data.Functor (($>))
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Swagger (Scheme (Http, Https), info, schemes, title, version)
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.User (AdminUser (..), User (..))
import qualified Database as DB
import Logic (registerWorkmode)
import Orphans ()
import Servant.API ((:<|>) (..), (:>), NoContent (..))
import Servant.Multipart (MultipartData (..), fdPayload)
import Servant.Server (Handler, ServerT, err400, errBody)
import qualified Servant.Server as S
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (swaggerSchemaUIServer)

type Server api = ServerT api (ReaderT Env Handler)

swaggerHandler :: S.Server SwaggerAPI
swaggerHandler = swaggerSchemaUIServer swagger
  where
    swagger =
      toSwagger (Proxy :: Proxy ("api" :> (API :<|> "admin" :> AdminAPI)))
        & schemes ?~ [Https, Http]
        & info . title .~ "Office Tracker API"
        & info . version .~ "1.0"

apiHandler :: Server ProtectedAPI
apiHandler user = (workmodeHandler user :<|> shiftHandler user :<|> officeHandler :<|> pure user) :<|> adminHandler

workmodeHandler :: User -> Server WorkmodeAPI
workmodeHandler user@(MkUser {email}) = regWorkmode :<|> flip confirmWorkmodeHandler :<|> DB.queryWorkmode email :<|> queryBatch
  where
    regWorkmode [] = pure NoContent
    regWorkmode (m : xs) = registerWorkmode user m >>= \case
      Right _ -> regWorkmode xs
      Left err -> throwError $ err400 {errBody = pack err}
    confirmWorkmodeHandler status = const (pure NoContent) <=< DB.confirmWorkmode email status <=< defaultDay
    queryBatch = withDefaultDays $ DB.queryWorkmodes email

shiftHandler :: User -> Server ShiftAPI
shiftHandler MkUser {email} = getShift :<|> setShift :<|> getShifts
  where
    getShift = listToMaybe <$> DB.getLastShiftsFor email
    getShifts office = filter ((==) office . shiftSite) . shifts <$> ask
    setShift x@(MkSetShift {site = office}) = do
      shiftNames <- fmap name <$> getShifts office
      if shiftName x `elem` shiftNames
        then DB.saveShift email x $> NoContent
        else throwError $ err400 {errBody = "specified shift does not exist"}

officeHandler :: Server OfficeAPI
officeHandler = getOffices :<|> getBooked
  where
    getOffices = offices <$> ask
    getBooked office = withDefaultDays $ DB.getOfficeBooked office

adminHandler :: AdminUser -> Server AdminAPI
adminHandler _ = shiftCSVAddHandler :<|> adminWorkmodeHandler :<|> DB.getPeople :<|> bookingsHandler :<|> contactsHandler
  where
    shiftCSVAddHandler = \case
      MultipartData [] [payload] -> CSV.saveShifts (fdPayload payload) >>= \case
        Left err -> throwError $ err400 {errBody = err}
        Right _ -> pure NoContent
      _ -> throwError $ err400 {errBody = "This endpoint only expects a single file and no other values"}
    bookingsHandler email = withDefaultDays $ DB.queryWorkmodes email
    contactsHandler email = withDefaultDays $ DB.queryContacts email

adminWorkmodeHandler :: Server AdminWorkmodeAPI
adminWorkmodeHandler = workmodeRangeHandler :<|> deleteWorkmodeHandler
  where
    workmodeRangeHandler office = withDefaultDays $ DB.getAllWorkmodes office
    deleteWorkmodeHandler = fmap (const NoContent) . DB.deleteWorkmodes

defaultDay :: MonadIO m => Maybe Day -> m Day
defaultDay = maybe (liftIO $ utctDay <$> getCurrentTime) pure

withDefaultDays :: MonadIO m => (Day -> Day -> m a) -> Maybe Day -> Maybe Day -> m a
withDefaultDays f startDate endDate = do
  start <- defaultDay startDate
  end <- defaultDay endDate
  f start end
