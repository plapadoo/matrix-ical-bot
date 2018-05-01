module IcalBot.AppointedTime(
    AppointedTime(..)
  , appTimeStart
  , appTimeStartUtc
  , appTimeEnd
  , appTimeEndUtc
  , appTimeStartAllDay
  , appTimeEndAllDay
  ) where

import           Data.Bool              (Bool (False))
import           Data.Eq                (Eq)
import           Data.Function          ((.))
import           Data.Functor           ((<$>))
import           Data.Maybe             (Maybe (Just, Nothing))
import           Data.Thyme.Clock       (UTCTime)
import           IcalBot.DateOrDateTime (DateOrDateTime, isAllDay,
                                         timeForDateTimeOrMidnight)
import           Text.Show              (Show)

-- |Either a point in time or a date range
data AppointedTime = OnlyStart DateOrDateTime
                   | Range DateOrDateTime DateOrDateTime
                  deriving(Show, Eq)

appTimeStart :: AppointedTime -> DateOrDateTime
appTimeStart (OnlyStart x) = x
appTimeStart (Range x _)   = x

appTimeStartAllDay :: AppointedTime -> Bool
appTimeStartAllDay (OnlyStart x) = isAllDay x
appTimeStartAllDay (Range x _)   = isAllDay x

appTimeEndAllDay :: AppointedTime -> Bool
appTimeEndAllDay (Range _ x) = isAllDay x
appTimeEndAllDay _           = False

appTimeStartUtc :: AppointedTime -> UTCTime
appTimeStartUtc = timeForDateTimeOrMidnight . appTimeStart

appTimeEnd :: AppointedTime -> Maybe DateOrDateTime
appTimeEnd (Range _ x) = Just x
appTimeEnd _           = Nothing

appTimeEndUtc :: AppointedTime -> Maybe UTCTime
appTimeEndUtc = (timeForDateTimeOrMidnight <$>) . appTimeEnd
