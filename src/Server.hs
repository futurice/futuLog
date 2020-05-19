module Server (officeHandler, Server) where

import API
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data
import Data.Aeson (ToJSON (..))
import Data.Functor (($>))
import Database (getAllWorkmodes, saveWorkmode)
import Servant.API ((:<|>) (..), NoContent (..))
import Servant.Server (Handler, ServerT, err404, err500)

type Server api = ServerT api (ReaderT Env Handler)

officeHandler :: Server OfficeAPI
officeHandler = workmodeHandler :<|> siteHandler

workmodeHandler :: Server WorkmodeAPI
workmodeHandler = registerWorkmode :<|> getAllWorkmodes
  where
    registerWorkmode m = saveWorkmode m $> NoContent

siteHandler :: Site -> Server RoomAPI
siteHandler site = getRooms site :<|> getRoom site
  where
    getRooms site = filter ((==) site . location) . rooms <$> ask
    getRoom site room = do
      rooms <- getRooms site
      case filter ((==) room . name) rooms of
        [] -> throwError err404
        [x] -> pure x
        _ -> throwError err500
