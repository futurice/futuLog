module Data.Workmode where

import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..), object, withObject)
import Data.Text (Text)

data Workmode = Office (Maybe Bool) | Leave | Home | Client Text
  deriving stock (Show, Eq)

instance ToJSON Workmode where
  toJSON (Office x) = object ["type" .= ("Office" :: Text), "confirmed" .= x]
  toJSON (Client x) = object ["type" .= ("Client" :: Text), "name" .= x]
  toJSON Leave = object ["type" .= ("Leave" :: Text)]
  toJSON Home = object ["type" .= ("Home" :: Text)]

instance FromJSON Workmode where
  parseJSON = withObject "Workmode" $ \obj -> do
    ty <- obj .: "type"
    case ty of
      "Home" -> pure Home
      "Leave" -> pure Leave
      "Office" -> pure $ Office Nothing
      "Client" -> Client <$> obj .: "name"
      _ -> fail $ "Wrong workmode specified: " <> ty
