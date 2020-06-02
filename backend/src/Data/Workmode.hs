module Data.Workmode where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..), object, withObject)
import Data.HashMap.Strict.InsOrd (fromList)
import Data.Proxy (Proxy (..))
import Data.Swagger (NamedSchema (..), SwaggerType (..), ToSchema (..), declareSchemaRef, properties, required, type_)
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

instance ToSchema Workmode where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy :: Proxy Text)
    boolSchema <- declareSchemaRef (Proxy :: Proxy (Maybe Bool))
    pure $ NamedSchema (Just "Workmode") $
      mempty
        & type_ ?~ SwaggerObject
        & properties
          .~ fromList
            [ ("type", stringSchema),
              ("confirmed", boolSchema),
              ("name", stringSchema)
            ]
        & required .~ ["type"]
