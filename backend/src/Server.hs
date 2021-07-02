{-# LANGUAGE StandaloneDeriving #-}
module Server (apiHandler, swaggerHandler, mkAuthServerContext, contextProxy, Server) where

import API
import Auth (contextProxy, mkAuthServerContext)
import Control.Monad.Identity (Identity)
import Data.Text (Text)
import Control.Lens ((&), (.~), (?~))
import Data.Errors (GenericError(..))
import Control.Monad ((<=<))
import Data.Functor ((<&>))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ClientRequest (Capacity (..), Office)
import Control.Monad.Reader (ReaderT)
import Data.Config (Shift (name), shiftSite)
import Data.Env (Env (..))
import Data.Functor (($>))
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Swagger (Scheme (Http, Https), info, schemes, title, version, ToSchema(..))
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.User (AdminUser (..), User (MkUser, email, isAdmin))
import qualified Database as DB
--import Logic (registerWorkmode)
import Servant.API ((:<|>) (..), (:>), NoContent (..), WithStatus(..), Union)
import Servant.Server (err400, errBody, respond, Handler)
import qualified Servant.Server as S
import Orphans ()
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (swaggerSchemaUIServer)
import Types (Server)

swaggerHandler :: S.Server SwaggerAPI
swaggerHandler = swaggerSchemaUIServer swagger
  where
    swagger =
      toSwagger (Proxy :: Proxy ("api" :> (API :<|> "admin" :> AdminAPI)))
        & schemes ?~ [Https, Http]
        & info . title .~ "Office Tracker API"
        & info . version .~ "1.0"

apiHandler :: Server ProtectedAPI
apiHandler user =
  (registrationsHandler user :<|> officesHandler :<|> meHandler user)
    :<|> adminHandler

meHandler :: User -> Server UserAPI
meHandler user = pure user :<|> setDefaultOffice
  where
    setDefaultOffice office = DB.setDefaultOffice user office >>= \case
        Just u-> respond $ WithStatus @200 u
        Nothing -> respond . WithStatus @400 $ MkGenericError @"No office with this name exists"

officesHandler :: Server OfficesAPI
officesHandler = DB.getOffices

registrationsHandler :: User -> Server RegistrationAPI
registrationsHandler user = register :<|> getRegistrations :<|> confirmRegistration
    where register = undefined
          getRegistrations = undefined
          confirmRegistration = undefined

{-workmodeHandler :: User -> Server WorkmodeAPI
workmodeHandler user@(MkUser {email}) = regWorkmode :<|> flip confirmWorkmodeHandler :<|> DB.queryWorkmode email :<|> queryBatch
  where
    regWorkmode [] = pure NoContent
    regWorkmode (m : xs) = registerWorkmode user m >>= \case
      Right _ -> regWorkmode xs
      Left err -> throwError $ err400 {errBody = pack err}
    confirmWorkmodeHandler status = const (pure NoContent) <=< DB.confirmWorkmode email status <=< defaultDay
    queryBatch = withDefaultDays $ DB.queryWorkmodes email
-}


{-officeHandler :: User -> Server OfficeAPI
officeHandler (MkUser {isAdmin}) = getOffices :<|> getBooked
  where
    getOffices = offices <$> ask
    getBooked office start end = withDefaultDays (DB.getOfficeBooked office) start end >>= \capacities -> do
      today <- liftIO $ utctDay <$> getCurrentTime
      if isAdmin
        then pure capacities
        else pure (fmap (\cap@(MkCapacity {date}) -> if date < today then cap {people = []} else cap) capacities)
-}

adminHandler :: AdminUser -> Server AdminAPI
adminHandler = undefined

{-adminHandler _ = addAdminHandler :<|> shiftCSVAddHandler :<|> adminWorkmodeHandler :<|> DB.getPeople :<|> bookingsHandler :<|> contactsHandler
  where
    shiftCSVAddHandler = \case
      MultipartData [] [payload] -> CSV.saveShifts (fdPayload payload) >>= \case
        Left err -> throwError $ err400 {errBody = err}
        Right _ -> pure NoContent
      _ -> throwError $ err400 {errBody = "This endpoint only expects a single file and no other values"}
    bookingsHandler email = withDefaultDays $ DB.queryWorkmodes email
    contactsHandler email = withDefaultDays $ DB.queryContacts email
    addAdminHandler email = DB.addAdmin email $> "Added user " <> email <> " as admin."

adminWorkmodeHandler :: Server AdminWorkmodeAPI
adminWorkmodeHandler = workmodeRangeHandler :<|> deleteWorkmodeHandler :<|> updateWorkmodeHandler
  where
    workmodeRangeHandler office = withDefaultDays $ DB.getAllWorkmodes office
    deleteWorkmodeHandler = fmap (const NoContent) . DB.deleteWorkmodes
    updateWorkmodeHandler = fmap (const NoContent) . DB.updateWorkmodes -}

defaultDay :: MonadIO m => Maybe Day -> m Day
defaultDay = maybe (liftIO $ utctDay <$> getCurrentTime) pure

withDefaultDays :: MonadIO m => (Day -> Day -> m a) -> Maybe Day -> Maybe Day -> m a
withDefaultDays f startDate endDate = do
  start <- defaultDay startDate
  end <- defaultDay endDate
  f start end
