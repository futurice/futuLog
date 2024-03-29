module Server (apiHandler, swaggerHandler, mkAuthServerContext, contextProxy, Server) where

import API
import Auth (contextProxy, mkAuthServerContext)
import Control.Lens ((&), (.~), (?~))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ClientRequest (Contacts, Office, RegistrationId (..), toRegistrations)
import Data.Errors (GenericError (..), RegistrationError (..))
import Data.Functor (($>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Swagger (Scheme (Http, Https), info, schemes, title, version)
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.User (AdminUser (..), Email, User (MkUser, email))
import qualified Database as DB
import Servant.API (NoContent (..), WithStatus (..), (:<|>) (..), (:>))
import Servant.Server (respond)
import qualified Servant.Server as S
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (swaggerSchemaUIServer)
import Types (Server)

(>$>) :: Functor f => (a -> f b) -> c -> a -> f c
f >$> c = \x -> f x $> c

swaggerHandler :: S.Server SwaggerAPI
swaggerHandler = swaggerSchemaUIServer swagger
  where
    swagger =
      toSwagger (Proxy :: Proxy ("api" :> (API :<|> "admin" :> AdminAPI)))
        & schemes ?~ [Https, Http]
        & info . title .~ "futuLog API"
        & info . version .~ "1.0"

apiHandler :: Server ProtectedAPI
apiHandler user =
  (registrationsHandler user :<|> officesHandler :<|> meHandler user)
    :<|> adminHandler

meHandler :: User -> Server UserAPI
meHandler user = pure user :<|> setDefaultOffice
  where
    setDefaultOffice office =
      DB.setDefaultOffice user office >>= \case
        Just u -> respond $ WithStatus @200 u
        Nothing -> respond . WithStatus @400 $ MkGenericError @"No office with this name exists"

officesHandler :: Server OfficesAPI
officesHandler = DB.getOffices :<|> bookingsHandler
  where
    bookingsHandler office = withDefaultDays $ \startDate endDate -> do
      today <- liftIO $ utctDay <$> getCurrentTime
      if startDate < today || endDate < today
        then
          respond @_ @'[WithStatus 200 [Contacts], WithStatus 400 (GenericError "Date may not be in the past")]
            . WithStatus @400
            $ MkGenericError @"Date may not be in the past"
        else DB.queryBooked office startDate endDate >>= respond . WithStatus @200

registrationsHandler :: User -> Server RegistrationAPI
registrationsHandler MkUser {email} = register :<|> getRegistrations :<|> confirmRegistration
  where
    register =
      DB.tryRegistrations Nothing email >=> pure . fmap MkRegistrationError >=> \case
        Nothing -> respond $ WithStatus @200 NoContent
        Just err -> respond $ WithStatus @400 err
    getRegistrations = withDefaultDays . DB.queryRegistrations $ email
    confirmRegistration :: Server ConfirmationAPI
    confirmRegistration =
      DB.confirmRegistration . MkRegistrationId email >=> \case
        True -> respond $ WithStatus @200 NoContent
        False -> respond . WithStatus @400 $ MkGenericError @"No registration for this day exists"

adminHandler :: AdminUser -> Server AdminAPI
adminHandler admin = adminsHandler admin :<|> adminRegistrationsHandler admin :<|> DB.getUsers :<|> adminOfficesHandler

adminsHandler :: AdminUser -> Server AdminsAPI
adminsHandler (MkAdminUser MkUser {email}) = DB.getAdmins :<|> putAdmin :<|> removeAdmin
  where
    putAdmin = DB.putAdmin >$> NoContent
    removeAdmin adminEmail =
      if email == adminEmail
        then respond . WithStatus @403 $ MkGenericError @"Cannot remove yourself as admin"
        else
          DB.removeAdmin adminEmail >>= \case
            Just e ->
              respond @_
                @'[ WithStatus 200 Email,
                    WithStatus 400 (GenericError "No admin with that email exists"),
                    WithStatus 403 (GenericError "Cannot remove yourself as admin")
                  ]
                $ WithStatus @200 e
            Nothing -> respond . WithStatus @400 $ MkGenericError @"No admin with that email exists"

adminRegistrationsHandler :: AdminUser -> Server AdminRegistrationAPI
adminRegistrationsHandler admin = deleteRegistrationsHandler :<|> editRegistrationsHandler :<|> adminUserHandler
  where
    deleteRegistrationsHandler =
      mapM DB.deleteRegistration >=> pure . catMaybes >=> \case
        [] -> respond $ WithStatus @200 NoContent
        xs -> respond $ WithStatus @400 xs
    editRegistrationsHandler = mapM (uncurry (DB.tryRegistrations (Just admin)) . toRegistrations) >$> NoContent

adminUserHandler :: Email -> Server AdminUserAPI
adminUserHandler email = userRegistrationsHandler :<|> contactsHandler
  where
    userRegistrationsHandler = withDefaultDays (DB.queryRegistrations email)
    contactsHandler = withDefaultDays (DB.queryContacts email)

adminOfficesHandler :: Server AdminOfficesAPI
adminOfficesHandler = setOfficeHandler :<|> (\office -> deleteOfficeHandler office :<|> getAdminBookingsHandler office)
  where
    setOfficeHandler = DB.setOffice >$> NoContent
    deleteOfficeHandler =
      DB.deleteOffice >=> \case
        Nothing -> respond @_ @'[WithStatus 200 Office, WithStatus 400 (GenericError "No office with that name exists")] . WithStatus @400 $ MkGenericError @"No office with that name exists"
        Just o -> respond $ WithStatus @200 o
    getAdminBookingsHandler office = withDefaultDays $ DB.queryBooked office

defaultDay :: MonadIO m => Maybe Day -> m Day
defaultDay = maybe (liftIO $ utctDay <$> getCurrentTime) pure

withDefaultDays :: MonadIO m => (Day -> Day -> m a) -> Maybe Day -> Maybe Day -> m a
withDefaultDays f startDate endDate = do
  start <- defaultDay startDate
  let end = fromMaybe start endDate
  f start end
