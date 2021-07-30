module Data.User (User (..), AdminUser (..), Admin (..), OpenIdUser (MkOpenIdUser), getUserEmail, Email(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.Swagger.ParamSchema (ToParamSchema)
import Servant (FromHttpApiData)

newtype Email = MkEmail Text
  deriving stock (Show)
  deriving newtype (Eq, FromJSON, ToJSON, ToSchema, FromField, ToField, ToParamSchema, FromHttpApiData)

data OpenIdUser
  = MkOpenIdUser
      { name :: Text,
        email :: Email,
        picture :: Text,
        defaultOffice :: Maybe Text,
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
        email :: Email,
        portrait :: Text,
        defaultOffice :: Maybe Text,
        isAdmin :: Bool
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema, FromRow, ToRow)

data Admin
  = MkAdmin
      { name :: Text,
        email :: Email
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema, FromRow)

getUserEmail :: User -> Email
getUserEmail MkUser {email} = email

newtype AdminUser = MkAdminUser User
  deriving stock (Show, Eq)
  deriving newtype (ToJSON, ToSchema)
