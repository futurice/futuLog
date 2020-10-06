module Data.User (User (..), AdminUser (..), FUMUser (MkFUMUser)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data FUMUser
  = MkFUMUser
      { first_name :: Text,
        last_name :: Text,
        email :: Text,
        portrait_full_url :: Text,
        portrait_thumb_url :: Text,
        portrait_badge_url :: Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data User
  = MkUser
      { first_name :: Text,
        last_name :: Text,
        email :: Text,
        portrait_full_url :: Text,
        portrait_thumb_url :: Text,
        portrait_badge_url :: Text,
        isAdmin :: Bool
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema, FromRow, ToRow)

newtype AdminUser = MkAdmin User
  deriving stock (Show, Eq)
  deriving newtype (ToJSON, FromJSON, ToSchema)

data Guest = MkGuest
    { first_name :: Text,
    last_name :: Text,
    email :: Text,
    phone :: Text }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema, FromRow, ToRow)
