module Data.Env where

import Data.Aeson (FromJSON, ToJSON)
import Data.Config (OfficeSpace, Shift)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import GHC.Generics (Generic)

data Env
  = MkEnv
      { offices :: [OfficeSpace],
        shifts :: [Shift],
        pool :: Pool Connection
      }
  deriving stock (Show)

data ShiftAssignment
  = MkShiftAssignment
      { userEmail :: Text,
        assignmentDate :: Day,
        site :: Text,
        shiftName :: Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, FromRow)

shiftAssignmentName :: ShiftAssignment -> Text
shiftAssignmentName = shiftName