module Types (Server) where

import Control.Monad.Reader (ReaderT)
import Data.Env (Env)
import Data.User (AdminUser, User)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server (Handler, ServerT)
import Servant.Server.Experimental.Auth (AuthServerData)

type Server api = ServerT api (ReaderT Env Handler)

type instance AuthServerData (AuthProtect "openid-connect") = User

type instance AuthServerData (AuthProtect "admin") = AdminUser
