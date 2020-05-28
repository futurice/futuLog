module Server (handler, Server) where

import API
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, ask)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ClientRequest (shiftName)
import Data.Config (name, shiftSite)
import Data.Env (Env (..))
import Data.Functor (($>))
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
shiftHandler office = setShift :<|> getShifts
  where
    getShifts = filter ((==) office . shiftSite) . shifts <$> ask
    setShift x = do
      shiftNames <- fmap name <$> getShifts
      if shiftName x `elem` shiftNames
        then saveShift office x $> NoContent
        else throwError $ err400 {errBody = "specified shift does not exist"}

officeHandler :: Server OfficeAPI
officeHandler = getOfficeCapacityOn
