module Data.Config where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Shift
  = MkShift
      { days :: [Int],
        name :: Text,
        site :: Text
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data OfficeSpace
  = MkOfficeSpace
      { site :: Text,
        maxPeople :: Int
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)
