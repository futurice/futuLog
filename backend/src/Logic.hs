module Logic (registerWorkmode) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.ClientRequest (RegisterWorkmode (..), people)
import Data.Config (Shift (..), maxPeople, officeSite, shiftSite)
import Data.Env (Env (..), ShiftAssignment (..))
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time.Calendar (dayOfWeek)
import Data.Workmode (Workmode (..))
import Database (getLastShiftsFor, getOfficeBooked, saveWorkmode)

registerWorkmode :: (MonadIO m, MonadReader Env m) => Text -> RegisterWorkmode -> m (Either String ())
registerWorkmode email mode@(MkRegisterWorkmode {workmode, site = office}) =
  if (not $ isOffice workmode)
    then Right <$> saveWorkmode email mode
    else do
      officeShifts <- getOfficeShifts office
      if null officeShifts
        then checkOfficeCapacity email mode
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
  shiftDays <- fmap days . listToMaybe . filter ((==) shiftName . name) <$> getOfficeShifts office
  case shiftDays of
    Just xs | fromEnum (dayOfWeek date) `elem` xs -> checkOfficeCapacity email mode
    _ -> pure $ Left "The date you tried to register for is not part of your shift"

checkOfficeCapacity :: (MonadIO m, MonadReader Env m) => Text -> RegisterWorkmode -> m (Either String ())
checkOfficeCapacity email mode@(MkRegisterWorkmode {date, site = office}) = do
  occupancy <- getOfficeBooked office date date
  maxCapacity <- maxPeople . head . filter ((==) office . officeSite) . offices <$> ask
  case occupancy of
    [x] | length (people x) < maxCapacity -> Right <$> saveWorkmode email mode
    [] -> Right <$> saveWorkmode email mode
    _ -> pure $ Left "Office already full on chosen day"

getOfficeShifts :: MonadReader Env m => Text -> m [Shift]
getOfficeShifts office = filter ((==) office . shiftSite) . shifts <$> ask
