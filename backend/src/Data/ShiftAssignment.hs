module Data.ShiftAssignment where

import Data.Csv ((.:), FromNamedRecord (..))
import Data.Text (Text)

data ShiftAssignment
  = MkShiftAssignment
      { userEmail :: Text,
        site :: Text,
        shiftName :: Text
      }
  deriving stock (Show, Eq)

instance FromNamedRecord ShiftAssignment where
  parseNamedRecord m =
    MkShiftAssignment
      <$> m .: "Email"
      <*> m .: "Office"
      <*> m .: "Shift name"
