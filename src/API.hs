module API where

import Data
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant.API

type OfficeAPI =
  "workmode" :> WorkmodeAPI
    :<|> "site" :> SiteAPI

type SiteAPI = Capture "name" Site :> "rooms" :> RoomAPI

type RoomAPI = Get '[JSON] [Room]

type WorkmodeAPI = "register" :> ReqBody '[JSON] RegisterWorkmode :> Post '[JSON] NoContent

data RegisterWorkmode
  = MkRegisterWorkmode
      { site :: Site,
        date :: UTCTime,
        workmode :: Workmode
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)
