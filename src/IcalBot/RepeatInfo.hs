module IcalBot.RepeatInfo(
  RepeatInfo(..)) where


import           Data.Eq                         (Eq)
import           IcalBot.AppointedTime           (AppointedTime)
import           Text.ICalendar.Types.Components (VEvent)
import           Text.Show                       (Show)

data RepeatInfo = RepeatInfo {
    riEvents   :: [VEvent]
  , riFirstRep :: AppointedTime
  } deriving(Eq,Show)
