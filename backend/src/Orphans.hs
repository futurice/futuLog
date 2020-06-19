{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where

import Control.Lens ((&), (?~))
import Data.Proxy (Proxy (..))
import Data.Swagger.Lens (required)
import Network.HTTP.Media.MediaType ((//))
import Servant.API ((:>))
import Servant.Multipart (MultipartData, MultipartForm)
import Servant.Swagger (HasSwagger (..))
import Servant.Swagger.Internal (addConsumes, addParam)

instance HasSwagger sub => HasSwagger ((MultipartForm a (MultipartData b)) :> sub) where
  toSwagger _ =
    toSwagger (Proxy :: Proxy sub)
      & addConsumes ["multipart" // "form-data"]
      & addParam param
    where
      param =
        mempty
          & required ?~ True
