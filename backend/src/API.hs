module API where

import Data.ClientRequest (AdminRegistration, Contacts, Office, Registration, RegistrationId)
import Data.Errors (GenericError, RegistrationError)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.User (Admin, Email, User)
import OpenID (OpenIDAPI)
import Servant.API
import Servant.Swagger.UI (SwaggerSchemaUI)

api :: Proxy (OpenIDAPI :<|> ProtectedAPI)
api = Proxy

rootAPI :: Proxy RootAPI
rootAPI = Proxy

type RootAPI = SwaggerAPI :<|> (OpenIDAPI :<|> ProtectedAPI) :<|> Raw

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

type ProtectedAPI =
  "api" :> AuthProtect "openid-connect"
    :> ( API
           :<|> AuthProtect "admin" :> "admin" :> AdminAPI
       )

type API =
  "registrations" :> RegistrationAPI
    :<|> "offices" :> OfficesAPI
    :<|> "me" :> UserAPI

type UserAPI =
  Get '[JSON] User
    :<|> ( Summary "Set the default office for the user"
             :> ReqBody '[JSON] Text
             :> UVerb
                  'PUT
                  '[JSON]
                  '[ WithStatus 200 User,
                     WithStatus 400 (GenericError "No office with this name exists")
                   ]
         )

type OfficesAPI =
  ( Summary "Get all defined offices with their capacity"
      :> Get '[JSON] [Office]
  )
    :<|> Capture "office" Text
      :> ( "bookings"
             :> Summary "Get the people that booked for a certain timespan (defaults to the current date)"
             :> QueryParam "startDate" Day
             :> QueryParam "endDate" Day
             :> UVerb
                  'GET
                  '[JSON]
                  '[ WithStatus 200 [Contacts],
                     WithStatus 400 (GenericError "Date may not be in the past")
                   ]
         )

type RegistrationAPI =
  ( Summary "Set where you work for a given day"
      :> ReqBody '[JSON] [Registration]
      :> UVerb
           'PUT
           '[JSON]
           '[ WithStatus 200 NoContent,
              WithStatus 400 RegistrationError
            ]
  )
    :<|> ( Summary "Get the place of work for the given days"
             :> Description "Only returns the days that were registered; start and end parameter default to the current day if omitted"
             :> QueryParam "startDate" Day
             :> QueryParam "endDate" Day
             :> Get '[JSON] [Registration]
         )
    :<|> ( "confirm"
             :> ConfirmationAPI
         )

type ConfirmationAPI =
  Summary "Set the confirmation flag for the given registration"
    :> ReqBody '[JSON] Day
    :> UVerb
         'PUT
         '[JSON]
         [ WithStatus 200 NoContent,
           WithStatus 400 (GenericError "No registration for this day exists")
         ]

type AdminAPI =
  "admins" :> AdminsAPI
    :<|> "registrations" :> AdminRegistrationAPI
    :<|> "people" :> Get '[JSON] [User]
    :<|> "offices" :> AdminOfficesAPI

type AdminsAPI =
  ( Summary "Get the list of all admins or the admins of an office"
      :> QueryParam "office" Text
      :> Get '[JSON] [Admin]
  )
    :<|> ( Summary "Add a new admin"
             :> ReqBody '[JSON] Email
             :> Put '[JSON] NoContent
         )
    :<|> ( Summary "Remove an admin"
             :> ReqBody '[JSON] Email
             :> UVerb
                  'DELETE
                  '[JSON]
                  [ WithStatus 200 Email,
                    WithStatus 400 (GenericError "No admin with that email exists")
                  ]
         )

type AdminOfficesAPI =
  ( Summary "Add a new office or update an existing office"
      :> ReqBody '[JSON] Office
      :> Put '[JSON] NoContent
  )
    :<|> ( Summary "Delete an office"
             :> ReqBody '[JSON] Text
             :> UVerb
                  'DELETE
                  '[JSON]
                  '[ WithStatus 200 Office,
                     WithStatus 400 (GenericError "No office with that name exists")
                   ]
         )

type AdminRegistrationAPI =
  ( Summary "Delete a given list of registrations"
      :> ReqBody '[JSON] [RegistrationId]
      :> UVerb
           'DELETE
           '[JSON]
           [ WithStatus 200 NoContent,
             WithStatus 400 [RegistrationId]
           ]
  )
    :<|> ( Summary "Edit or set registrations"
             :> ReqBody '[JSON] [AdminRegistration]
             :> Put '[JSON] NoContent
         )
    :<|> Capture "user" Email
      :> AdminUserAPI

type AdminUserAPI =
  ( Summary "Get all the registrations of the user in a given timeframe"
      :> QueryParam "startDate" Day
      :> QueryParam "endDate" Day
      :> Get '[JSON] [Registration]
  )
    :<|> ( "contacts"
             :> Summary "Get all users that were in contact with the user in a given timeframe"
             :> QueryParam "startDate" Day
             :> QueryParam "endDate" Day
             :> Get '[JSON] [Contacts]
         )
