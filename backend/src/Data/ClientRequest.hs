{-# LANGUAGE RecordWildCards #-}

module Data.ClientRequest where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.User (Email, User (..))
import Data.Workmode (Workmode (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (Null (Null))
import GHC.Generics (Generic)

data Registration = MkRegistration
  { office :: Text,
    date :: Day,
    workmode :: Workmode
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

registrationDate :: Registration -> Day
registrationDate = date

registrationOffice :: Registration -> Text
registrationOffice = office

data AdminRegistration = MkAdminRegistration
  { office :: Text,
    date :: Day,
    email :: Email,
    workmode :: Workmode
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

toRegistration :: AdminRegistration -> (Email, Registration)
toRegistration MkAdminRegistration {..} = (email, MkRegistration {..})

data RegistrationId = MkRegistrationId
  { date :: Day,
    email :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Contacts = MkContacts
  { date :: Day,
    office :: Text,
    people :: [User]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Office = MkOffice
  { name :: Text,
    capacity :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema, FromRow)

data UserRegistration = MkUserRegistration User Registration

instance ToRow UserRegistration where
  toRow (MkUserRegistration MkUser {email} MkRegistration {office, date, workmode}) =
    [toField email, toField office, toField date] <> case workmode of
      Office b -> [toField ("Office" :: Text), toField b, toField Null]
      Client x -> [toField ("Client" :: Text), toField Null, toField x]
      Leave -> [toField ("Leave" :: Text), toField Null, toField Null]
      Home -> [toField ("Home" :: Text), toField Null, toField Null]

instance FromRow Registration where
  fromRow = do
    void $ field @Text
    common <- MkRegistration <$> field <*> field
    mode <- field
    common <$> case mode of
      "Office" -> Office <$> field <* nullField
      "Client" -> Client <$> (nullField *> field)
      "Leave" -> Leave <$ nullField <* nullField
      "Home" -> Home <$ nullField <* nullField
      _ -> error $ "Could not deserialize working mode type: " <> mode
    where
      nullField = field @Null
