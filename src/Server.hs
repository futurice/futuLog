module Server (officeHandler) where

import API
import Control.Monad.IO.Class (liftIO)
import Data
import Data.Aeson (ToJSON (..))
import Servant.API ((:<|>) (..), NoContent (..))
import Servant.Server (Server)

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
    getRooms _ = pure [MkRoom {location = Munich, name = "Marienplatz", maxPeople = 2}]
    addRoom = undefined
