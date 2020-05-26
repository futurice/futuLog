module Data.Env where

import Data.Config (OfficeSpace, Shift)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

data Env
  = MkEnv
      { offices :: [OfficeSpace],
        shifts :: [Shift],
        pool :: Pool Connection
      }
  deriving stock (Show)
