{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans where

import Data.Swagger (ToSchema(..))
import Servant.API (WithStatus)
import Data.Proxy (Proxy(..))

instance ToSchema a => ToSchema (WithStatus s a) where
    declareNamedSchema _ = declareNamedSchema (Proxy @a)
