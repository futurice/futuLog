module Server (apiHandler, swaggerHandler, mkAuthServerContext, contextProxy, Server) where

import API
import Auth (contextProxy, mkAuthServerContext)
import Control.Lens ((&), (.~), (?~))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ClientRequest (RegistrationId (..), toRegistration)
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
officesHandler = DB.getOffices :<|> bookedHandler
  where
    bookedHandler office date = do
      today <- liftIO $ utctDay <$> getCurrentTime
      let d = fromMaybe today date
      if d < today
        then
          respond @_ @'[WithStatus 200 [User], WithStatus 400 (GenericError "Date may not be in the past")]
            . WithStatus @400
            $ MkGenericError @"Date may not be in the past"
        else DB.queryBooked office d >>= respond . WithStatus @200

registrationsHandler :: User -> Server RegistrationAPI
registrationsHandler user@MkUser {email} = register :<|> getRegistrations :<|> confirmRegistration
  where
    register =
      DB.tryRegistrations user >=> pure . fmap MkRegistrationError >=> \case
        Nothing -> respond $ WithStatus @200 NoContent
        Just err -> respond $ WithStatus @400 err
    getRegistrations = withDefaultDays . DB.queryRegistrations $ email
    confirmRegistration :: Server ConfirmationAPI
    confirmRegistration =
      DB.confirmRegistration . MkRegistrationId email >=> \case
        True -> respond $ WithStatus @200 NoContent
        False -> respond . WithStatus @400 $ MkGenericError @"No registration for this day exists"

adminHandler :: AdminUser -> Server AdminAPI
adminHandler _ = adminsHandler :<|> adminRegistrationsHandler :<|> DB.getUsers

adminsHandler :: Server AdminsAPI
adminsHandler = DB.getAdmins :<|> putAdmin :<|> removeAdmin
  where
    putAdmin = DB.putAdmin >$> NoContent
    removeAdmin email =
      DB.removeAdmin email >>= \case
        Just e -> respond @_ @'[WithStatus 200 Email, WithStatus 400 (GenericError "No admin with that email exists")] $ WithStatus @200 e
        Nothing -> respond . WithStatus @400 $ MkGenericError @"No admin with that email exists"

adminRegistrationsHandler :: Server AdminRegistrationAPI
adminRegistrationsHandler = deleteRegistrationsHandler :<|> editRegistrationsHandler :<|> adminUserHandler
  where
    deleteRegistrationsHandler =
      mapM DB.deleteRegistration >=> pure . catMaybes >=> \case
        [] -> respond $ WithStatus @200 NoContent
        xs -> respond $ WithStatus @400 xs
    editRegistrationsHandler = mapM (uncurry DB.saveRegistration . toRegistration) >$> NoContent

adminUserHandler :: Email -> Server AdminUserAPI
adminUserHandler email = userRegistrationsHandler :<|> contactsHandler
  where
    userRegistrationsHandler = withDefaultDays (DB.queryRegistrations email)
    contactsHandler = withDefaultDays (DB.queryContacts email)

defaultDay :: MonadIO m => Maybe Day -> m Day
defaultDay = maybe (liftIO $ utctDay <$> getCurrentTime) pure

withDefaultDays :: MonadIO m => (Day -> Day -> m a) -> Maybe Day -> Maybe Day -> m a
withDefaultDays f startDate endDate = do
  start <- defaultDay startDate
  end <- defaultDay endDate
  f start end
