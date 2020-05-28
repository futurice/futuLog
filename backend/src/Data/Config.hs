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

shiftSite :: Shift -> Text
shiftSite = site

data OfficeSpace
  = MkOfficeSpace
      { site :: Text,
        maxPeople :: Int
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

officeSite :: OfficeSpace -> Text
officeSite = site
