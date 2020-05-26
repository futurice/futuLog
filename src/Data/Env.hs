module Data.Env where

import Data.Aeson (FromJSON, ToJSON)
import Data.Config (OfficeSpace, Shift)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
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
        shiftName :: Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

shiftAssignmentName :: ShiftAssignment -> Text
shiftAssignmentName = shiftName

instance FromRow ShiftAssignment where
  fromRow = MkShiftAssignment <$> field <*> field <*> field
