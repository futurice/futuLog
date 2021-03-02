module Types (Server) where

import Control.Monad.Reader (ReaderT)
import Data.Env (Env)
import Servant.Server (Handler, ServerT)

type Server api = ServerT api (ReaderT Env Handler)
