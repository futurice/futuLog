module Data where

import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..), object, withObject)
import Data.Binary.Builder (fromByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Pool (Pool)
import Data.Text (Text, toLower)
import Data.Time.LocalTime (TimeOfDay)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (Action (Plain), ToField (..))
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData (..))

data Env
  = MkEnv
      { rooms :: [Room],
        pool :: Pool Connection
      }
  deriving stock (Show)

data Site = Munich | Berlin | Stuttgart
  deriving stock (Generic, Show, Eq, Enum)
  deriving anyclass (ToJSON, FromJSON)

data Workmode = Office Text Timeslot | Home | Leave | Client Text
  deriving stock (Show, Eq)

data Room
  = MkRoom
      { location :: Site,
        name :: Text,
        maxPeople :: Int
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data Timeslot
  = MkTimeslot
      { startTime :: TimeOfDay,
        endTime :: TimeOfDay
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

-- custom instances
instance FromHttpApiData Site where
  parseUrlPiece s
    | toLower s == "munich" = Right Munich
    | toLower s == "berlin" = Right Berlin
    | toLower s == "stuttgart" = Right Stuttgart
    | otherwise = Left $ "Unknown site: " <> s

instance ToField Site where
  toField s = Plain . fromByteString . pack $ "'" <> show s <> "'"

instance FromField Site where
  fromField f dat = fromField f dat >>= \case
    ("Munich" :: String) -> pure Munich
    "Berlin" -> pure Berlin
    "Stuttgart" -> pure Stuttgart

instance ToJSON Workmode where
  toJSON (Office name slot) = object ["type" .= ("Office" :: Text), "timeslot" .= slot, "roomName" .= name]
  toJSON Home = object ["type" .= ("Home" :: Text)]
  toJSON Leave = object ["type" .= ("Leave" :: Text)]
  toJSON (Client name) = object ["type" .= ("Client" :: Text), "name" .= name]

instance FromJSON Workmode where
  parseJSON = withObject "Workmode" $ \obj -> do
    t <- obj .: "type"
    case (t :: Text) of
      "Office" -> Office <$> obj .: "roomName" <*> obj .: "timeslot"
      "Home" -> pure Home
      "Leave" -> pure Leave
      "Client" -> Client <$> obj .: "name"
      _ -> fail "'type' is none of the allowed workmodes"
