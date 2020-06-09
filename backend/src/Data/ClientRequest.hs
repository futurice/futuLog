module Data.ClientRequest where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Workmode (Workmode (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field)
import Database.PostgreSQL.Simple.Types (Null)
import GHC.Generics (Generic)

data SetShift
  = MkSetShift
      {shiftName :: Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RegisterWorkmode
  = MkRegisterWorkmode
      { site :: Text,
        date :: Day,
        workmode :: Workmode
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data UserWorkmode
  = MkUserWorkmode
      { userEmail :: Text,
        site :: Text,
        date :: Day,
        workmode :: Workmode
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

workmodeSite :: RegisterWorkmode -> Text
workmodeSite = site

workmodeDate :: RegisterWorkmode -> Day
workmodeDate = date

instance FromRow UserWorkmode where
  fromRow = do
    common <- MkUserWorkmode <$> field <*> field <*> field
    mode <- field
    common <$> case mode of
      "Office" -> Office <$> field <* nullField
      "Client" -> Client <$> (nullField *> field)
      "Leave" -> Leave <$ nullField <* nullField
      "Home" -> Home <$ nullField <* nullField
      _ -> error $ "Could not deserialize working mode type: " <> mode
    where
      nullField = field :: RowParser Null