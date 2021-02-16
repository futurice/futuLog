module Logic (registerWorkmode) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.ClientRequest (Capacity (people), RegisterWorkmode (..))
import Data.Config (Shift (..), maxPeople, officeSite, shiftSite)
import Data.Env (Env (..), ShiftAssignment (..))
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time.Calendar (dayOfWeek)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.User (User (MkUser, email))
import Data.Workmode (Workmode (..))
import Database (getLastShiftsFor, getOfficeBooked, saveWorkmode)

registerWorkmode :: (MonadIO m, MonadReader Env m) => User -> RegisterWorkmode -> m (Either String ())
registerWorkmode user@(MkUser {email}) mode@(MkRegisterWorkmode {workmode, site = office, date}) = do
  today <- liftIO $ utctDay <$> getCurrentTime
  if date < today
    then pure $ Left "You can not change workmodes in the past, please ask an admin"
    else
      if (not $ isOffice workmode)
        then Right <$> saveWorkmode user mode
        else do
          officeShifts <- getOfficeShifts office
          if null officeShifts
            then checkOfficeCapacity user mode
            else do
              shifts <- getLastShiftsFor email
              case shifts of
                [] -> pure $ Left "You are not signed into any shift, please set a shift first"
                shift : _ -> checkShift user mode shift
  where
    isOffice (Office _) = True
    isOffice _ = False

checkShift :: (MonadIO m, MonadReader Env m) => User -> RegisterWorkmode -> ShiftAssignment -> m (Either String ())
checkShift user mode@(MkRegisterWorkmode {site = office, date}) (MkShiftAssignment {shiftName}) = do
  shiftDays <- fmap days . listToMaybe . filter ((==) shiftName . name) <$> getOfficeShifts office
  case shiftDays of
    Just xs | fromEnum (dayOfWeek date) `elem` xs -> checkOfficeCapacity user mode
    _ -> pure $ Left "The date you tried to register for is not part of your shift"

checkOfficeCapacity :: (MonadIO m, MonadReader Env m) => User -> RegisterWorkmode -> m (Either String ())
checkOfficeCapacity user mode@(MkRegisterWorkmode {date, site = office}) = do
  occupancy <- getOfficeBooked office date date
  maxCapacity <- maxPeople . head . filter ((==) office . officeSite) . offices <$> ask
  case occupancy of
    [x] | length (people x) < maxCapacity -> Right <$> saveWorkmode user mode
    [] -> Right <$> saveWorkmode user mode
    _ -> pure $ Left "Office already full on chosen day"

getOfficeShifts :: MonadReader Env m => Text -> m [Shift]
getOfficeShifts office = filter ((==) office . shiftSite) . shifts <$> ask
