module API where

import Data.ClientRequest (RegisterWorkmode, SetShift)
import Data.Config (Shift)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Servant.API

type API =
  "workmode" :> WorkmodeAPI
    :<|> "shift" :> ShiftAPI
    :<|> "office" :> OfficeAPI

type WorkmodeAPI =
  "register" :> ReqBody '[JSON] RegisterWorkmode :> Post '[JSON] NoContent
    :<|> "all" :> Get '[JSON] [RegisterWorkmode] -- DEVELOPMENT ONLY

type ShiftAPI =
  "set" :> ReqBody '[JSON] SetShift :> Post '[JSON] NoContent
    :<|> "all" :> Capture "site" Text :> Get '[JSON] [Shift]

type OfficeAPI = Capture "site" Text :> "capacity" :> Capture "date" Day :> Get '[JSON] Int
