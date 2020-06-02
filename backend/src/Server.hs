module Server (handler, Server) where

import API
import Control.Lens ((&), (.~), (?~))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, ask)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ClientRequest (shiftName)
import Data.Config (Shift (name), shiftSite)
import Data.Env (Env (..))
import Data.Functor (($>))
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Swagger (Scheme (Http), Swagger, info, schemes, title, version)
import Database (getAllWorkmodes, getLastShiftsFor, getOfficeCapacityOn, saveShift)
import Logic (registerWorkmode)
import Servant.API ((:<|>) (..), (:>), NoContent (..))
import Servant.Server (Handler, ServerT, err400, errBody)
import Servant.Swagger (toSwagger)

type Server api = ServerT api (ReaderT Env Handler)

swagger :: Swagger
swagger =
  toSwagger (Proxy :: Proxy ("api" :> API))
    & schemes ?~ [Http]
    & info . title .~ "Office Tracker API"
    & info . version .~ "1.0"

handler :: Server RootAPI
handler = pure swagger :<|> apiHandler

apiHandler :: Server API
apiHandler = workmodeHandler :<|> shiftHandler :<|> officeHandler

workmodeHandler :: Server WorkmodeAPI
workmodeHandler = regWorkmode :<|> getAllWorkmodes
  where
    regWorkmode m = registerWorkmode m >>= \case
      Right _ -> pure NoContent
      Left err -> throwError $ err400 {errBody = pack err}

shiftHandler :: Server ShiftAPI
shiftHandler = getShift :<|> (\office -> setShift office :<|> getShifts office)
  where
    getShift (Just user) = listToMaybe <$> getLastShiftsFor user
    getShift Nothing = pure Nothing
    getShifts office = filter ((==) office . shiftSite) . shifts <$> ask
    setShift office x = do
      shiftNames <- fmap name <$> getShifts office
      if shiftName x `elem` shiftNames
        then saveShift office x $> NoContent
        else throwError $ err400 {errBody = "specified shift does not exist"}

officeHandler :: Server OfficeAPI
officeHandler = getOfficeCapacityOn
