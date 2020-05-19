module Server (officeHandler, Env (..), Server) where

import API
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data
import Data.Aeson (ToJSON (..))
import Servant.API ((:<|>) (..), NoContent (..))
import Servant.Server (Handler, ServerT)

data Env
  = MkEnv
      { rooms :: [Room]
      }
  deriving stock (Show, Eq)

type Server api = ServerT api (ReaderT Env Handler)

officeHandler :: Server OfficeAPI
officeHandler = workmodeHandler :<|> siteHandler

workmodeHandler :: Server WorkmodeAPI
workmodeHandler MkRegisterWorkmode {site, date, workmode} = do
  liftIO $ print site
  liftIO $ print date
  liftIO $ print workmode
  pure NoContent

siteHandler :: Site -> Server RoomAPI
siteHandler site = getRooms site :<|> addRoom
  where
    getRooms site = filter ((==) site . location) . rooms <$> ask
    addRoom = undefined
