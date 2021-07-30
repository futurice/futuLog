module Data.Env where

import Data.Aeson (FromJSON, ToJSON)
import Data.ClientRequest (Office)
import Data.Pool (Pool)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import OpenID.Connect.Client.Provider (Provider)

data Env
  = MkEnv
      { offices :: [Office],
        pool :: Pool Connection,
        manager :: Manager,
        provider :: Provider
      }

data ShiftAssignment
  = MkShiftAssignment
      { userEmail :: Text,
        assignmentDate :: Day,
        site :: Text,
        shiftName :: Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, FromRow, ToSchema)

shiftAssignmentName :: ShiftAssignment -> Text
shiftAssignmentName = shiftName

shiftAssignmentSite :: ShiftAssignment -> Text
shiftAssignmentSite = site
