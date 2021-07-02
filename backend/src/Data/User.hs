module Data.User (User (..), AdminUser (..), Admin (..), OpenIdUser (MkOpenIdUser), getUserEmail) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data OpenIdUser
  = MkOpenIdUser
      { name :: Text,
        email :: Text,
        picture :: Text,
        expire :: Maybe UTCTime,
        accessToken :: Maybe Text,
        refreshToken :: Maybe Text,
        rawIdToken :: Maybe Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToRow, FromRow)

data User
  = MkUser
      { name :: Text,
        email :: Text,
        portrait :: Text,
        isAdmin :: Bool
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema, FromRow, ToRow)

data Admin
  = MkAdmin
      { name :: Text,
        email :: Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

getUserEmail :: User -> Text
getUserEmail MkUser {email} = email

newtype AdminUser = MkAdminUser User
  deriving stock (Show, Eq)
  deriving newtype (ToJSON, ToSchema)
