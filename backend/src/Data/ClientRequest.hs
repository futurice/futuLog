{-# LANGUAGE OverloadedLists #-}

module Data.ClientRequest where

import Data.Aeson (FromJSON, ToJSON)
import Data.Csv ((.=), DefaultOrdered (..), ToNamedRecord (..), namedRecord)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.User (User)
import Data.Workmode (Workmode (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field)
import Database.PostgreSQL.Simple.Types (Null)
import GHC.Generics (Generic)

data SetShift
  = MkSetShift
      { shiftName :: Text,
        site :: Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RegisterWorkmode
  = MkRegisterWorkmode
      { site :: Text,
        date :: Day,
        workmode :: Workmode,
        note :: Maybe Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AdminWorkmode
  = MkAdminWorkmode
      { site :: Text,
        date :: Day,
        email :: Text,
        workmode :: Workmode,
        note :: Maybe Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WorkmodeId
  = MkWorkmodeId
      { date :: Day,
        email :: Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data UserWorkmode
  = MkUserWorkmode
      { userEmail :: Text,
        site :: Text,
        date :: Day,
        workmode :: Workmode,
        note :: Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Capacity
  = MkCapacity
      { date :: Day,
        people :: [User]
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Contact
  = MkContact
      { date :: Day,
        site :: Text,
        people :: [User]
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
    common <$> specific mode <*> field
    where
      nullField = field :: RowParser Null
      specific "Office" = Office <$> field <* nullField
      specific "Client" = Client <$> (nullField *> field)
      specific "Leave" = Leave <$ nullField <* nullField
      specific "Home" = Home <$ nullField <* nullField
      specific mode = error $ "Could not deserialize working mode type: " <> mode

instance DefaultOrdered UserWorkmode where
  headerOrder _ = ["Email", "Date", "Office"]

instance ToNamedRecord UserWorkmode where
  toNamedRecord (MkUserWorkmode {userEmail, site, date}) =
    namedRecord
      [ "Email" .= userEmail,
        "Date" .= iso8601Show date,
        "Office" .= site
      ]
