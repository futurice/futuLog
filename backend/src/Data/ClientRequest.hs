{-# LANGUAGE OverloadedLists #-}

module Data.ClientRequest where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.User (User)
import Data.Workmode (Workmode (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field)
import Database.PostgreSQL.Simple.Types (Null)
import GHC.Generics (Generic)

data Registration
  = MkRegistration
      { site :: Text,
        date :: Day,
        workmode :: Workmode
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AdminRegistration
  = MkAdminRegistration
      { site :: Text,
        date :: Day,
        email :: Text,
        workmode :: Workmode
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data RegistrationId
  = MkRegistrationId
      { date :: Day,
        email :: Text
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

data Office
  = MkOffice
      { name :: Text,
        capacity :: Int
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema, FromRow)
{-instance FromRow UserWorkmode where
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

instance DefaultOrdered UserWorkmode where
  headerOrder _ = ["Email", "Date", "Office"]

instance ToNamedRecord UserWorkmode where
  toNamedRecord (MkUserWorkmode {userEmail, site, date}) =
    namedRecord
      [ "Email" .= userEmail,
        "Date" .= iso8601Show date,
        "Office" .= site
      ] -}
