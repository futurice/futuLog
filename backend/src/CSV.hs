module CSV (saveShifts) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.CSV (ShiftAssignment (..))
import Data.ClientRequest (SetShift (..))
import Data.Csv (decodeByName)
import Data.Env (Env)
import Database (saveShift)

saveShifts :: (MonadIO m, MonadReader Env m) => ByteString -> m (Either ByteString ())
saveShifts p = case decodeByName p of
  Left err -> pure $ Left $ pack err
  Right (_, shifts) -> fmap Right $ forM_ shifts $ \MkShiftAssignment {userEmail, site, shiftName} ->
    saveShift userEmail site MkSetShift {shiftName = shiftName}
