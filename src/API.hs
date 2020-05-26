module API where

import Data.ClientRequest (RegisterWorkmode, SetShift)
import Data.Text (Text)
import Servant.API

type API =
  "workmode" :> WorkmodeAPI
    :<|> "shift" :> ShiftAPI
    :<|> "office" :> OfficeAPI

type WorkmodeAPI =
  "register" :> ReqBody '[JSON] RegisterWorkmode :> Post '[JSON] NoContent
    :<|> "all" :> Get '[JSON] [RegisterWorkmode] -- DEVELOPMENT ONLY

type ShiftAPI = ReqBody '[JSON] SetShift :> Post '[JSON] NoContent

type OfficeAPI = Capture "site" Text :> "capacity" :> Get '[JSON] Int
