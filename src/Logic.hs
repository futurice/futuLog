module Logic (registerWorkmode) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.ClientRequest (RegisterWorkmode (..))
import Data.Config (Shift (..), maxPeople, officeSite, shiftSite)
import Data.Env (Env (..), ShiftAssignment (..))
import Data.Time.Calendar (dayOfWeek)
import Data.Workmode (Workmode (..))
import Database (getLastShiftsFor, getOfficeCapacityOn, saveWorkmode)

registerWorkmode :: (MonadIO m, MonadReader Env m) => RegisterWorkmode -> m (Either String ())
registerWorkmode mode@(MkRegisterWorkmode {userEmail, workmode}) =
  if (not $ isOffice workmode)
    then Right <$> saveWorkmode mode
    else do
      shifts <- getLastShiftsFor userEmail
      case shifts of
        [] -> pure $ Left "You are not signed into any shift, please set a shift first"
        [shift] -> checkShift mode shift
        _ -> pure $ Left "You cannot visit the office, you have to stay home two weeks after changing shifts"
  where
    isOffice (Office _) = True
    isOffice _ = False

checkShift :: (MonadIO m, MonadReader Env m) => RegisterWorkmode -> ShiftAssignment -> m (Either String ())
checkShift mode@(MkRegisterWorkmode {site = office, date}) (MkShiftAssignment {shiftName}) = do
  shiftDays <- days . head . filter ((==) shiftName . name) . filter ((==) office . shiftSite) . shifts <$> ask
  if not (fromEnum (dayOfWeek date) `elem` shiftDays)
    then pure $ Left "The date you tried to register for is not part of your shift"
    else do
      occupancy <- getOfficeCapacityOn office date
      maxCapacity <- maxPeople . head . filter ((==) office . officeSite) . offices <$> ask
      if occupancy < maxCapacity
        then Right <$> saveWorkmode mode
        else pure $ Left "Office already full on chosen day"
