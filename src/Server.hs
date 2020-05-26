module Server (handler, Server) where

import API
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Aeson (ToJSON (..))
import Data.ByteString.Lazy.Char8 (pack)
import Data.Config (OfficeSpace (..), Shift (..), shiftSite)
import Data.Env (Env (..))
import Data.Functor (($>))
import Data.Text (Text)
import Database (getAllWorkmodes, getOfficeCapacityOn, saveShift)
import Logic (registerWorkmode)
import Servant.API ((:<|>) (..), NoContent (..))
import Servant.Server (Handler, ServerT, err400, errBody)

type Server api = ServerT api (ReaderT Env Handler)

handler :: Server API
handler = workmodeHandler :<|> shiftHandler :<|> officeHandler

workmodeHandler :: Server WorkmodeAPI
workmodeHandler = regWorkmode :<|> getAllWorkmodes
  where
    regWorkmode m = registerWorkmode m >>= \case
      Right _ -> pure NoContent
      Left err -> throwError $ err400 {errBody = pack err}

shiftHandler :: Server ShiftAPI
shiftHandler = setShift :<|> getShifts
  where
    setShift x = saveShift x $> NoContent
    getShifts office = filter ((==) office . shiftSite) . shifts <$> ask

officeHandler :: Server OfficeAPI
officeHandler = getOfficeCapacityOn
