module API where

import Data.ClientRequest (RegisterWorkmode, SetShift)
import Data.Config (OfficeSpace, Shift)
import Data.Env (ShiftAssignment)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Servant.API
import Servant.Swagger.UI (SwaggerSchemaUI)

api :: Proxy ProtectedAPI
api = Proxy

rootAPI :: Proxy (SwaggerAPI :<|> ProtectedAPI)
rootAPI = Proxy

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

type ProtectedAPI = "api" :> AuthProtect "fum-cookie" :> API

type API =
  "workmode" :> WorkmodeAPI
    :<|> "shift" :> ShiftAPI
    :<|> "office" :> OfficeAPI

type WorkmodeAPI =
  "register" :> ReqBody '[JSON] RegisterWorkmode :> Post '[JSON] NoContent
    :<|> "get" :> Capture "date" Day :> Get '[JSON] (Maybe RegisterWorkmode)
    :<|> "all" :> Get '[JSON] [RegisterWorkmode] -- DEVELOPMENT ONLY

type ShiftAPI =
  "get" :> Get '[JSON] (Maybe ShiftAssignment)
    :<|> Capture "site" Text
      :> ( "set" :> ReqBody '[JSON] SetShift :> Post '[JSON] NoContent
             :<|> "all" :> Get '[JSON] [Shift]
         )

type OfficeAPI =
  "all" :> Get '[JSON] [OfficeSpace]
    :<|> Capture "site" Text :> "capacity" :> Capture "date" Day :> Get '[JSON] Int
