module Server (handler, Server) where

import API
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, ask)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ClientRequest (shiftName)
import Data.Config (Shift (name), shiftSite)
import Data.Env (Env (..))
import Data.Functor (($>))
import Data.Maybe (listToMaybe)
import Database (getAllWorkmodes, getLastShiftsFor, getOfficeCapacityOn, saveShift)
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
shiftHandler = getShift :<|> (\office -> setShift office :<|> getShifts office)
  where
    getShift user = listToMaybe <$> getLastShiftsFor user
    getShifts office = filter ((==) office . shiftSite) . shifts <$> ask
    setShift office x = do
      shiftNames <- fmap name <$> getShifts office
      if shiftName x `elem` shiftNames
        then saveShift office x $> NoContent
        else throwError $ err400 {errBody = "specified shift does not exist"}

officeHandler :: Server OfficeAPI
officeHandler = getOfficeCapacityOn
