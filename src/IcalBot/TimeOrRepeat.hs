module IcalBot.TimeOrRepeat(
  TimeOrRepeat(..)) where

import           Data.Eq               (Eq)
import           IcalBot.AppointedTime (AppointedTime)
import           IcalBot.RepeatInfo    (RepeatInfo)
import           Text.Show             (Show)

data TimeOrRepeat = Time AppointedTime
                  | Repeat RepeatInfo
                  deriving(Show, Eq)
