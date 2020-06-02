module API where

import Data.ClientRequest (RegisterWorkmode, SetShift)
import Data.Config (Shift)
import Data.Env (ShiftAssignment)
import Data.Proxy (Proxy (..))
import Data.Swagger (Swagger)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Servant.API

api :: Proxy RootAPI
api = Proxy

type RootAPI =
  "swagger.json" :> Get '[JSON] Swagger
    :<|> "api" :> API

type API =
  "workmode" :> WorkmodeAPI
    :<|> "shift" :> ShiftAPI
    :<|> "office" :> OfficeAPI

type WorkmodeAPI =
  "register" :> ReqBody '[JSON] RegisterWorkmode :> Post '[JSON] NoContent
    :<|> "all" :> Get '[JSON] [RegisterWorkmode] -- DEVELOPMENT ONLY

type ShiftAPI =
  "get" :> QueryParam "userEmail" Text :> Get '[JSON] (Maybe ShiftAssignment)
    :<|> Capture "site" Text
      :> ( "set" :> ReqBody '[JSON] SetShift :> Post '[JSON] NoContent
             :<|> "all" :> Get '[JSON] [Shift]
         )

type OfficeAPI = Capture "site" Text :> "capacity" :> Capture "date" Day :> Get '[JSON] Int
