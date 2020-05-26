module Server (handler, Server) where

import API
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Aeson (ToJSON (..))
import Data.Config (OfficeSpace (..))
import Data.Env (Env (..))
import Data.Functor (($>))
import Data.Text (Text)
import Database (getAllWorkmodes, saveWorkmode)
import Servant.API ((:<|>) (..), NoContent (..))
import Servant.Server (Handler, ServerT, err404, err500)

type Server api = ServerT api (ReaderT Env Handler)

handler :: Server API
handler = workmodeHandler :<|> shiftHandler :<|> officeHandler

workmodeHandler :: Server WorkmodeAPI
workmodeHandler = registerWorkmode :<|> getAllWorkmodes
  where
    registerWorkmode m = saveWorkmode m $> NoContent

shiftHandler :: Server ShiftAPI
shiftHandler setShift = pure NoContent

officeHandler :: Server OfficeAPI
officeHandler office = getCapacity office
  where
    getCapacity office = maxPeople . head . filter ((==) office . site) . offices <$> ask
