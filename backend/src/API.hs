module API where

import Data.ClientRequest (AdminWorkmode, Capacity, Contact, Registration, SetShift, UserWorkmode, WorkmodeId)
import Data.Config (OfficeSpace, Shift)
import Data.Env (ShiftAssignment)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.User (User)
import OpenID (OpenIDAPI)
import Servant.API
import Servant.CSV.Cassava (CSV)
import Servant.Multipart (Mem, MultipartData, MultipartForm)
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
    :<|> "offices" :> Get '[JSON] [Office]
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

type RegistrationAPI =
  ( Summary "Set where you work for a given day"
      :> ReqBody '[JSON] [NewRegistration]
      :> UVerb 'PUT '[JSON]
           '[ WithStatus 200 [Registration],
              WithStatus 409 RegistrationError
            ]
  )
    :<|> ( Summary "Get the place of work for the given days"
             :> Description "Only returns the days that were registered; start and end parameter default to the current day if omitted"
             :> QueryParam "startDate" Day
             :> QueryParam "endDate" Day
             :> Get '[JSON] [Registration]
         )
    :<|> Capture "id" RegistrationId
      :> ( "confirm"
             :> Summary "Set the confirmation flag for the given registration"
             :> UVerb 'PUT '[JSON]
                  [ WithStatus 200 NoContent,
                    WithStatus 400 (GenericError "No registration with this id exists")
                  ]
         )

type AdminAPI =
  "admins" :> AdminsAPI
    :<|> "registrations" :> AdminRegistrationApi
    :<|> "people" :> Get '[JSON] [User]
    :<|> "bookings" :> Capture "user" Text :> QueryParam "startDate" Day :> QueryParam "endDate" Day :> Get '[JSON] [UserWorkmode]
    :<|> "contacts" :> Capture "user" Text :> QueryParam "startDate" Day :> QueryParam "endDate" Day :> Get '[JSON] [Contact]

type AdminsAPI =
  ( Summary "Get the list of all admins or the admins of an office"
      :> QueryParam "office"
      :> Get '[JSON] [Admin]
  )
    :<|> ( Summary "Add a new admin or change their office"
             :> ReqBody '[JSON] Admin
             :> Put '[JSON] NoContent
         )
    :<|> Capture "email" Text
      :> ( Summary "Remove an admin"
             :> UVerb 'DELETE '[JSON]
                  [ WithStatus 200 Admin,
                    WithStatus 400 (GenericError "No admin with that email exists")
                  ]
         )

type AdminWorkmodeAPI =
  ( Summary "Delete a given list of registrations"
      :> ReqBody '[JSON] [RegistrationId]
      :> UVerb 'DELETE '[JSON]
           [ WithStatus 200 NoContent,
             WithStatus 400 [RegistrationId]
           ]
  )
    :<|> "update" :> ReqBody '[JSON] [AdminWorkmode] :> Put '[JSON] NoContent
