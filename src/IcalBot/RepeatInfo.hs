module IcalBot.RepeatInfo(
  RepeatInfo(..)) where


import           Data.Eq                         (Eq, (==))
import           Data.Function                   ((.))
import           IcalBot.AppointedTime           (AppointedTime)
import           Text.ICalendar.Types.Components (VEvent)
import           Text.Show                       (Show, show)

data RepeatInfo = RepeatInfo {
    riEvents   :: [VEvent]
  , riFirstRep :: AppointedTime
  }

instance Eq RepeatInfo where
  a == b = riFirstRep a == riFirstRep b

instance Show RepeatInfo where
  show = show . riFirstRep
