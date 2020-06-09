module Data.User where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data User
  = MkUser
      { first_name :: Text,
        last_name :: Text,
        email :: Text,
        portrait_full_url :: Text,
        portrait_thumb_url :: Text,
        portrait_badge_url :: Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype AdminUser = MkAdmin User
  deriving stock (Show, Eq)
  deriving newtype (ToJSON, FromJSON, ToSchema)
