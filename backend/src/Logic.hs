module Logic (registerWorkmode) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.ClientRequest (RegisterWorkmode (..))
import Data.Config (Shift (..), maxPeople, officeSite, shiftSite)
import Data.Env (Env (..), ShiftAssignment (..))
import Data.Text (Text)
import Data.Time.Calendar (dayOfWeek)
import Data.Workmode (Workmode (..))
import Database (getLastShiftsFor, getOfficeCapacityOn, saveWorkmode)

registerWorkmode :: (MonadIO m, MonadReader Env m) => Text -> RegisterWorkmode -> m (Either String ())
registerWorkmode email mode@(MkRegisterWorkmode {workmode}) =
  if (not $ isOffice workmode)
    then Right <$> saveWorkmode email mode
    else do
      shifts <- getLastShiftsFor email
      case shifts of
        [] -> pure $ Left "You are not signed into any shift, please set a shift first"
        [shift] -> checkShift email mode shift
        _ -> pure $ Left "You cannot visit the office, you have to stay home two weeks after changing shifts" --TODO: Check if person was in the office in the last two weeks
  where
    isOffice (Office _) = True
    isOffice _ = False

checkShift :: (MonadIO m, MonadReader Env m) => Text -> RegisterWorkmode -> ShiftAssignment -> m (Either String ())
checkShift email mode@(MkRegisterWorkmode {site = office, date}) (MkShiftAssignment {shiftName}) = do
  shiftDays <- days . head . filter ((==) shiftName . name) . filter ((==) office . shiftSite) . shifts <$> ask
  if not (fromEnum (dayOfWeek date) `elem` shiftDays)
    then pure $ Left "The date you tried to register for is not part of your shift"
    else do
      occupancy <- getOfficeCapacityOn office date
      maxCapacity <- maxPeople . head . filter ((==) office . officeSite) . offices <$> ask
      if occupancy < maxCapacity
        then Right <$> saveWorkmode email mode
        else pure $ Left "Office already full on chosen day"
