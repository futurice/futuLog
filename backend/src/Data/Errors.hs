{-# LANGUAGE OverloadedLists #-}

module Data.Errors where

import Data.Char (toLower)
import Data.Function ((&))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Optics ((?~), (.~))
import Data.Text (Text, pack)
import Data.Aeson ((.=), ToJSON (..), Value (String), object)
import Data.Time.Calendar (Day)
import Data.Swagger (NamedSchema(..), ToSchema(..), declareSchemaRef, SwaggerType(SwaggerObject))
import Data.Swagger.Optics ()
import Data.Proxy (Proxy(..))

data GenericError (msg :: Symbol) = MkGenericError

instance (KnownSymbol msg) => Show (GenericError msg) where
    show _ = "GenericError \"" <> symbolVal (Proxy @msg) <> "\""

instance (KnownSymbol msg) => ToJSON (GenericError msg) where
    toJSON _ = object [ "error" .= symbolVal (Proxy @msg) ]

instance (KnownSymbol msg) => ToSchema (GenericError msg) where
    declareNamedSchema _ = do
        stringSchema <- declareSchemaRef (Proxy @Text)
        pure $ NamedSchema Nothing $ mempty
            & #type ?~ SwaggerObject
            & #description ?~ msg
            & #properties .~ [("error", stringSchema)]
        where msg = case symbolVal (Proxy @msg) of
                        [] -> "Generic error"
                        (x:xs) -> "Error when there is " <> pack (toLower x : xs)

newtype RegistrationError = MkRegistrationError [Day]
  deriving stock (Show)

registrationErrorMessage :: Text
registrationErrorMessage = "The following days could not be registered as the office is already full. Aborted registration."

instance ToJSON RegistrationError where
  toJSON (MkRegistrationError days) =
    object
      [ "message" .= String registrationErrorMessage,
        "days" .= toJSON days
      ]

instance ToSchema RegistrationError where
    declareNamedSchema _ = do
        daysSchema <- declareSchemaRef (Proxy @[Day])
        stringSchema <- declareSchemaRef (Proxy @Text)
        pure $ NamedSchema (Just "RegistrationError") $ mempty
            & #type ?~ SwaggerObject
            & #description ?~ "Error when not all days are available because the office is full."
            & #properties .~
                [ ("message", stringSchema),
                  ("days", daysSchema)
                ]
