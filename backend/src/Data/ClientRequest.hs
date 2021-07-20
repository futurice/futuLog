{-# LANGUAGE RecordWildCards #-}

module Data.ClientRequest where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.User (User, Email)
import Data.Workmode (Workmode (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.Types (Null)
import GHC.Generics (Generic)
import Control.Monad (void)

data Registration
  = MkRegistration
      { office :: Text,
        date :: Day,
        workmode :: Workmode
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AdminRegistration
  = MkAdminRegistration
      { office :: Text,
        date :: Day,
        email :: Email,
        workmode :: Workmode
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

toRegistration :: AdminRegistration -> (Email, Registration)
toRegistration MkAdminRegistration {..} = (email, MkRegistration {..})

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
    where nullField = field @Null
