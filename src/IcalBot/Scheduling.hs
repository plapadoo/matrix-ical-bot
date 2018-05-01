{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
-- |Module containing all functions pertaining to scheduling (upcoming events)
module IcalBot.Scheduling(
    collectAppts
  , nextMessage) where

import           Data.Function          (on, (.))
import           Data.Functor           ((<$>))
import           Data.List              (concatMap, filter, sortBy)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (Maybe, mapMaybe)
import           Data.Monoid            ((<>))
import           Data.Ord               (compare, (>=))
import           Data.Thyme.Calendar    (Day)
import           Data.Thyme.Clock       (UTCTime)
import           Data.Time.Zones        (TZ)
import           Data.Tuple             (fst)
import           IcalBot.Appt           (apptTimeOfDayStart)
import           IcalBot.DateOrDateTime (timeForDateTime)
import           IcalBot.EventDB        (EventDB, collectFlat, daysGrouped)
import           IcalBot.Formatting     (formatSubApptNow, formatTodayAppts,
                                         formatTomorrowAppts)
import           IcalBot.MatrixMessage  (MatrixMessage)
import           IcalBot.SubAppt        (SubAppt (saAppt, saTime),
                                         appointmentDates)
import           IcalBot.Util           (headSafe, hour, makeUtcTime)
import           Prelude                (pred)

-- |The next message for an appointment
-- nextAppt :: EventDB -> UTCTime -> Maybe MatrixMessage
-- nextAppt db now =
--   let appts = collectDirectAppts False db
--       filtered = filter ((>= now) . fst) appts
--       sorted = sortBy (compare `on` fst) filtered
--   in snd <$> headSafe sorted

-- |When is the next message to be sent (doesn't have to be an appointment)
nextMessage :: EventDB -> TZ -> UTCTime -> Maybe (UTCTime, MatrixMessage)
nextMessage db tz now =
  let appts = collectAppts db tz now
      sorted = sortBy (compare `on` fst) appts
  in headSafe sorted

-- |Collect all messages for a single day
mkDayMessages :: TZ -> (Day, [SubAppt]) -> [(UTCTime, MatrixMessage)]
mkDayMessages tz (day, appts) =
  let sortedAppts :: [SubAppt]
      sortedAppts = sortBy (compare `on` (apptTimeOfDayStart . saAppt)) appts
      beforeTime :: UTCTime
      beforeTime = makeUtcTime (pred day) (hour 20)
      onTime :: UTCTime
      onTime = makeUtcTime day (hour 8)
      warningBefore = formatTomorrowAppts tz sortedAppts
      warningOn = formatTodayAppts tz sortedAppts
  in [(beforeTime, warningBefore), (onTime, warningOn)]

-- |Collect future appointments and pre-appointment messages
collectAppts :: EventDB -> TZ -> UTCTime -> [(UTCTime, MatrixMessage)]
collectAppts db tz now =
  let -- |First, collect all starts/ends of all appointments
      subAppts :: [SubAppt]
      subAppts = collectFlat db appointmentDates
      -- Then, filter out those that are all day, and generate messages for the rest
      mkPointMessage :: SubAppt -> Maybe (UTCTime, MatrixMessage)
      mkPointMessage sa = (, formatSubApptNow sa) <$> timeForDateTime (saTime sa)
      -- But take back our all-day messages by grouping by day and emitting day messages
      dayAppts :: [(UTCTime, MatrixMessage)]
      dayAppts = concatMap (mkDayMessages tz) (Map.toList (daysGrouped db))
      allAppts :: [(UTCTime, MatrixMessage)]
      allAppts = mapMaybe mkPointMessage subAppts <> dayAppts
  in filter ((>= now) . fst) allAppts
