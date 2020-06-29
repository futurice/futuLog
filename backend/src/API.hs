module API where

import Data.ClientRequest (Capacity, RegisterWorkmode, SetShift, UserWorkmode)
import Data.Config (OfficeSpace, Shift)
import Data.Env (ShiftAssignment)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.User (User)
import Servant.API
import Servant.Multipart (Mem, MultipartData, MultipartForm)
import Servant.Swagger.UI (SwaggerSchemaUI)

api :: Proxy ProtectedAPI
api = Proxy

rootAPI :: Proxy RootAPI
rootAPI = Proxy

type RootAPI = SwaggerAPI :<|> ProtectedAPI :<|> Raw

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

type ProtectedAPI =
  "api" :> AuthProtect "fum-cookie"
    :> ( API
           :<|> AuthProtect "admin" :> "admin" :> AdminAPI
       )

type API =
  "workmode" :> WorkmodeAPI
    :<|> "shift" :> ShiftAPI
    :<|> "office" :> OfficeAPI
    :<|> "me" :> Get '[JSON] User

type WorkmodeAPI =
  "register" :> ReqBody '[JSON] [RegisterWorkmode] :> Post '[JSON] NoContent
    :<|> "confirm" :> QueryParam "date" Day :> ReqBody '[JSON] Bool :> Post '[JSON] NoContent
    :<|> "get" :> Capture "date" Day :> Get '[JSON] (Maybe UserWorkmode)
    :<|> "batch" :> QueryParam "startDate" Day :> QueryParam "endDate" Day :> Get '[JSON] [UserWorkmode]
    :<|> "all" :> Capture "office" Text :> QueryParam "date" Day :> Get '[JSON] [Text]

type ShiftAPI =
  "get" :> Get '[JSON] (Maybe ShiftAssignment)
    :<|> Capture "site" Text
      :> ( "set" :> ReqBody '[JSON] SetShift :> Post '[JSON] NoContent
             :<|> "all" :> Get '[JSON] [Shift]
         )

type OfficeAPI =
  "all" :> Get '[JSON] [OfficeSpace]
    :<|> Capture "site" Text :> "booked" :> QueryParam "startDate" Day :> QueryParam "endDate" Day :> Get '[JSON] [Capacity]

type AdminAPI = "shift" :> "csv" :> "add" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] NoContent
