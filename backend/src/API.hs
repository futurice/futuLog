module API where

import Data.ClientRequest (AdminRegistration, Contact, Office, Registration, RegistrationId)
import Data.Errors (GenericError, RegistrationError)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.User (Admin, User)
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
             :> UVerb 'PUT '[JSON]
                  '[ WithStatus 200 User,
                     WithStatus 400 (GenericError "No office with this name exists")
                   ]
         )

type OfficesAPI = Get '[JSON] [Office]

type RegistrationAPI =
  ( Summary "Set where you work for a given day"
      :> ReqBody '[JSON] [Registration]
      :> UVerb 'PUT '[JSON]
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
    :<|> ReqBody '[JSON] RegistrationId
      :> ( "confirm"
             :> Summary "Set the confirmation flag for the given registration"
             :> UVerb 'PUT '[JSON]
                  [ WithStatus 200 NoContent,
                    WithStatus 400 (GenericError "No registration with this id exists")
                  ]
         )

type AdminAPI =
  "admins" :> AdminsAPI
    :<|> "registrations" :> AdminRegistrationAPI
    :<|> "people" :> Get '[JSON] [User]

type AdminsAPI =
  ( Summary "Get the list of all admins or the admins of an office"
      :> QueryParam "office" Text
      :> Get '[JSON] [Admin]
  )
    :<|> ( Summary "Add a new admin or change their office"
             :> ReqBody '[JSON] Admin
             :> Put '[JSON] Admin --NoContent
         )
    :<|> Capture "email" Text
      :> ( Summary "Remove an admin"
             :> UVerb 'DELETE '[JSON]
                  [ WithStatus 200 Admin,
                    WithStatus 400 (GenericError "No admin with that email exists")
                  ]
         )

type AdminRegistrationAPI =
  ( Summary "Delete a given list of registrations"
      :> ReqBody '[JSON] [RegistrationId]
      :> UVerb 'DELETE '[JSON]
           [ WithStatus 200 NoContent,
             WithStatus 400 [RegistrationId]
           ]
  )
    :<|> ( Summary "Edit a set of registrations"
             :> ReqBody '[JSON] [AdminRegistration]
             :> UVerb 'PUT '[JSON]
                  [ WithStatus 200 NoContent,
                    WithStatus 400 [RegistrationId]
                  ]
         )
    :<|> Capture "user" Text
      :> ( ( Summary "Get all the registrations of the user in a given timeframe"
               :> QueryParam "startDate" Day
               :> QueryParam "endDate" Day
               :> UVerb 'GET '[JSON]
                    [ WithStatus 200 [Registration],
                      WithStatus 400 (GenericError "No user with that email exists")
                    ]
           )
             :<|> ( "contacts"
                      :> Summary "Get all users that were in contact with the user in a given timeframe"
                      :> QueryParam "startDate" Day
                      :> QueryParam "endDate" Day
                      :> UVerb 'GET '[JSON]
                           [ WithStatus 200 [Contact],
                             WithStatus 400 (GenericError "No user with that email exists")
                           ]
                  )
         )
