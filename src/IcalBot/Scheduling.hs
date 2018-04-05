-- |Module containing all functions pertaining to scheduling (upcoming events)
module IcalBot.Scheduling(
    collectAppointments
  , nextAppointment) where

import           Control.Applicative   (pure)
import           Control.Lens          (view, (^.))
import           Data.Bool             (otherwise)
import           Data.Function         (on)
import           Data.Functor          ((<$>))
import           Data.List             (concatMap, sortBy)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (Maybe)
import           Data.Monoid           (mempty, (<>))
import           Data.Ord              (compare)
import qualified Data.Text             as Text
import           Data.Thyme.Calendar   (Day)
import           Data.Thyme.Clock      (UTCTime)
import           Data.Time.Zones       (TZ)
import           Data.Tuple            (fst)
import           IcalBot.Appointment   (AppointedTime (OnlyStart, Range),
                                        Appointment,
                                        DateOrDateTime (AllDay, AtPoint),
                                        ieAllDay, ieSummary, ieTime,
                                        ieTimeOfDayStart,
                                        ieTimeOfDayStartAtZone)
import           IcalBot.EventDB       (EventDB, collectFlat, daysGrouped,
                                        filterDBAfter)
import           IcalBot.Formatting    (timeOfDayToText)
import           IcalBot.MatrixMessage (MatrixMessage, plainMessage)
import           IcalBot.Util          (headSafe, hour, makeUtcTime)
import           Prelude               (pred)

nextAppointment :: EventDB -> TZ -> UTCTime -> Maybe (UTCTime, MatrixMessage)
nextAppointment db tz now =
  let appts = collectAppointments db tz now
      sorted = sortBy (compare `on` fst) appts
  in headSafe sorted

collectAppointments :: EventDB -> TZ -> UTCTime -> [(UTCTime, MatrixMessage)]
collectAppointments db tz now =
  let filtered :: EventDB
      filtered = filterDBAfter db now
      collectAppointment :: Appointment -> [(UTCTime, MatrixMessage)]
      collectAppointment a =
        case a ^. ieTime of
          OnlyStart (AllDay _) -> mempty
          OnlyStart (AtPoint p) ->
            pure (p, plainMessage ("Beginnt jetzt: " <> a ^. ieSummary))
          Range s e ->
            let start =
                  case s of
                    AtPoint p -> pure (p, plainMessage ("Beginnt jetzt: " <> a ^. ieSummary))
                    _ -> mempty
                end =
                  case e of
                    AtPoint p -> pure (p, plainMessage ("Endet jetzt: " <> a ^. ieSummary))
                    _ -> mempty
            in start <> end
      formatApptShort :: Appointment -> Text.Text
      formatApptShort a | a ^. ieAllDay = a ^. ieSummary <> " (ganztägig)"
                        | otherwise = timeOfDayToText (a ^. (ieTimeOfDayStartAtZone tz)) <> " " <> a ^. ieSummary
      makeDay :: (Day, [Appointment]) -> [(UTCTime, MatrixMessage)]
      makeDay (day, appts) =
        let sortedAppts :: [Appointment]
            sortedAppts = sortBy (compare `on` (view ieTimeOfDayStart)) appts
            beforeTime :: UTCTime
            beforeTime = makeUtcTime (pred day) (hour 20)
            onTime :: UTCTime
            onTime = makeUtcTime day (hour 8)
            warningBefore =
              case sortedAppts of
                [x] -> plainMessage ("Termin morgen: " <> formatApptShort x)
                xs -> plainMessage ("Mehrere Termine morgen: " <> Text.intercalate ", " (formatApptShort <$> xs))
            warningOn =
              case sortedAppts of
                [x] -> plainMessage ("Termin heute: " <> formatApptShort x)
                xs -> plainMessage ("Mehrere Termine heute: " <> Text.intercalate ", " (formatApptShort <$> xs))
        in [(beforeTime, warningBefore), (onTime, warningOn)]
      directAppts :: [(UTCTime, MatrixMessage)]
      directAppts = collectFlat filtered collectAppointment
      dayAppts :: [(UTCTime, MatrixMessage)]
      dayAppts = concatMap makeDay (Map.toList (daysGrouped filtered))
  in directAppts <> dayAppts
