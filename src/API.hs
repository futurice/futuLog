module API where

import Data
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field)
import Database.PostgreSQL.Simple.Types (Null)
import GHC.Generics (Generic)
import Servant.API

type OfficeAPI =
  "workmode" :> WorkmodeAPI
    :<|> "site" :> SiteAPI

type SiteAPI = Capture "name" Site :> "rooms" :> RoomAPI

type RoomAPI =
  Get '[JSON] [Room]
    :<|> Capture "name" Text :> Get '[JSON] Room

type WorkmodeAPI =
  "register" :> ReqBody '[JSON] RegisterWorkmode :> Post '[JSON] NoContent
    :<|> "all" :> Get '[JSON] [RegisterWorkmode]

data RegisterWorkmode
  = MkRegisterWorkmode
      { userEmail :: Text,
        fullName :: Text,
        site :: Site,
        date :: Day,
        workmode :: Workmode
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance FromRow RegisterWorkmode where
  fromRow = do
    common <- MkRegisterWorkmode <$> field <*> field <*> field <*> field
    mode <- field
    case (mode :: String) of
      "Home" -> replicateNull 4 $> common Home
      "Leave" -> replicateNull 4 $> common Leave
      "Client" -> replicateNull 3 *> (common . Client <$> field)
      "Office" -> common <$> (Office <$> field <*> (MkTimeslot <$> field <*> field <* nullField))
    where
      nullField = field :: RowParser Null
      replicateNull 0 = pure ()
      replicateNull n = nullField *> replicateNull (n - 1)
