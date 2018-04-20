-- |Module containing all functions pertaining to scheduling (upcoming events)
module IcalBot.Scheduling(
    collectAppointments
  , nextAppointment
  , nextMessage) where

import           Control.Applicative   (pure)
import           Data.Function         (on, (.))
import           Data.Functor          ((<$>))
import           Data.List             (concatMap, filter, sortBy)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (Maybe, fromJust)
import           Data.Monoid           (mempty, (<>))
import           Data.Ord              (compare, (>=))
import qualified Data.Text             as Text
import           Data.Thyme.Calendar   (Day)
import           Data.Thyme.Clock      (UTCTime)
import           Data.Time.Zones       (TZ)
import           Data.Tuple            (fst)
import           IcalBot.Appointment   (AppointedTime (OnlyStart, Range),
                                        Appointment,
                                        DateOrDateTime (AllDay, AtPoint),
                                        ieAllDay, ieAllDayEnd, ieSummary,
                                        ieTime, ieTimeOfDayEndAtZone,
                                        ieTimeOfDayStart,
                                        ieTimeOfDayStartAtZone)
import           IcalBot.EventDB       (AppointmentStatus (AppointmentEnds, AppointmentStarts),
                                        EventDB, SelectedAppointment,
                                        collectFlat, daysGrouped, saAppt,
                                        saStatus)
import           IcalBot.Formatting    (timeOfDayToText)
import           IcalBot.MatrixMessage (MatrixMessage, plainMessage)
import           IcalBot.Util          (headSafe, hour, makeUtcTime)
import           Prelude               (pred, undefined)

nextAppointment = undefined

-- |When is the next message to be sent (doesn't have to be an appointment)
nextMessage :: EventDB -> TZ -> UTCTime -> Maybe (UTCTime, MatrixMessage)
nextMessage db tz now =
  let appts = collectAppointments db tz now
      sorted = sortBy (compare `on` fst) appts
  in headSafe sorted

collectAppointment :: Appointment -> [(UTCTime, MatrixMessage)]
collectAppointment a =
  case ieTime a of
    OnlyStart (AllDay _) -> mempty
    OnlyStart (AtPoint p) ->
      pure (p, plainMessage ("Beginnt jetzt: " <> ieSummary a))
    Range s e ->
      let start =
            case s of
              AtPoint p -> pure (p, plainMessage ("Beginnt jetzt: " <> ieSummary a))
              _ -> mempty
          end =
            case e of
              AtPoint p -> pure (p, plainMessage ("Endet jetzt: " <> ieSummary a))
              _ -> mempty
      in start <> end

formatApptShort :: TZ -> SelectedAppointment -> Text.Text
formatApptShort tz a =
  let appt = saAppt a
  in
    case saStatus a of
      AppointmentStarts ->
        if ieAllDay appt
        then ieSummary appt <> " (ganztägig)"
        else timeOfDayToText (ieTimeOfDayStartAtZone tz appt) <> " " <> ieSummary appt
      AppointmentEnds ->
        if ieAllDayEnd appt
        then ieSummary appt <> " endet"
        else timeOfDayToText (fromJust (ieTimeOfDayEndAtZone tz appt)) <> " " <> ieSummary appt <> " endet"

makeDay :: TZ -> (Day, [SelectedAppointment]) -> [(UTCTime, MatrixMessage)]
makeDay tz (day, appts) =
  let sortedAppts :: [SelectedAppointment]
      sortedAppts = sortBy (compare `on` (ieTimeOfDayStart . saAppt)) appts
      beforeTime :: UTCTime
      beforeTime = makeUtcTime (pred day) (hour 20)
      onTime :: UTCTime
      onTime = makeUtcTime day (hour 8)
      warningBefore =
        case sortedAppts of
          [x] -> plainMessage ("Termin morgen: " <> formatApptShort tz x)
          xs -> plainMessage ("Mehrere Termine morgen: " <> Text.intercalate ", " (formatApptShort tz <$> xs))
      warningOn =
        case sortedAppts of
          [x] -> plainMessage ("Termin heute: " <> formatApptShort tz x)
          xs -> plainMessage ("Mehrere Termine heute: " <> Text.intercalate ", " (formatApptShort tz <$> xs))
  in [(beforeTime, warningBefore), (onTime, warningOn)]

collectAppointments :: EventDB -> TZ -> UTCTime -> [(UTCTime, MatrixMessage)]
collectAppointments db tz now =
  let directAppts :: [(UTCTime, MatrixMessage)]
      directAppts = collectFlat db collectAppointment
      dayAppts :: [(UTCTime, MatrixMessage)]
      dayAppts = concatMap (makeDay tz) (Map.toList (daysGrouped db))
  in filter ((>= now) . fst) (directAppts <> dayAppts)
