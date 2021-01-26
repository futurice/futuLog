module Data.User (User (..), AdminUser (..), ContactUser (MkContactUser)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data ContactUser
  = MkContactUser
      { first :: Text,
        name :: Text,
        email :: Text
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
