module Data.ClientRequest where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Workmode (Workmode (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field)
import Database.PostgreSQL.Simple.Types (Null)
import GHC.Generics (Generic)

data SetShift
  = MkSetShift
      { userEmail :: Text,
        shiftName :: Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data RegisterWorkmode
  = MkRegisterWorkmode
      { userEmail :: Text,
        site :: Text,
        date :: Day,
        workmode :: Workmode
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

workmodeSite :: RegisterWorkmode -> Text
workmodeSite = site

workmodeDate :: RegisterWorkmode -> Day
workmodeDate = date

instance FromRow RegisterWorkmode where
  fromRow = do
    common <- MkRegisterWorkmode <$> field <*> field <*> field
    mode <- field
    common <$> case mode of
      "Office" -> Office <$> field <* nullField
      "Client" -> Client <$> (nullField *> field)
      "Leave" -> Leave <$ nullField <* nullField
      "Home" -> Home <$ nullField <* nullField
      _ -> error $ "Could not deserialize working mode type: " <> mode
    where
      nullField = field :: RowParser Null
