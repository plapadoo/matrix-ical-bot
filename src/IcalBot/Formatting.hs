{-# LANGUAGE OverloadedStrings #-}
-- |Functions to format the internal data structures and date/times
module IcalBot.Formatting(
    dayToText
  , localTimeToText
  , formatDateOrDateTime
  , timeOfDayToText
  , formatTime
  , formatEventAsText
  , formatSubApptNow
  , formatApptShort
  , formatDiffAsText
  , formatDiffs
  , textShow
  , formatTomorrowAppts
  , formatTodayAppts
  ) where

import           Control.Lens           ((^.))
import           Data.Function          (($), (.))
import           Data.Functor           ((<$>))
import           Data.Maybe             (Maybe (Just, Nothing), fromJust)
import           Data.Monoid            ((<>))
import           Data.Ord               ((<))
import           Data.String            (IsString (fromString))
import qualified Data.Text              as Text
import           Data.Thyme.Calendar    (Day, gregorian, _ymdDay, _ymdMonth,
                                         _ymdYear)
import           Data.Thyme.LocalTime   (LocalTime, TimeOfDay (..), _localDay,
                                         _localTimeOfDay)
import           Data.Time.Zones        (TZ)
import           IcalBot.AppointedTime  (AppointedTime (OnlyStart, Range),
                                         appTimeEndAllDay, appTimeStartAllDay)
import           IcalBot.Appt           (Appt, apptSummary, apptTime,
                                         apptTimeOfDayEndAtZone,
                                         apptTimeOfDayStartAtZone)
import           IcalBot.ApptStatus     (ApptStatus (ApptEnds, ApptStarts))
import           IcalBot.DateOrDateTime (DateOrDateTime (AllDay, AtPoint))
import           IcalBot.EventDB        (EventDifference (..))
import           IcalBot.MatrixMessage  (MatrixMessage, plainMessage)
import           IcalBot.RepeatInfo     (riFirstRep)
import           IcalBot.SubAppt        (SubAppt (saAppt, saStatus))
import           IcalBot.TimeOrRepeat   (TimeOrRepeat (Repeat, Time))
import           IcalBot.Util           (utcTimeAtTz)
import           Text.Show              (Show, show)

dayToText :: Day -> Text.Text
dayToText day =
  let ymd = day ^. gregorian
  in Text.pack $ show (ymd ^. _ymdDay) <> "." <> show (ymd ^. _ymdMonth) <> "." <> show (ymd ^. _ymdYear)

localTimeToText :: LocalTime -> Text.Text
localTimeToText t =
  let dayText :: Text.Text
      dayText = dayToText (t ^. _localDay)
      dayTimeInt = t ^. _localTimeOfDay
      dt :: Text.Text
      dt = timeOfDayToText dayTimeInt
  in dayText <> " " <> dt

formatDateOrDateTime :: TZ -> DateOrDateTime -> Text.Text
formatDateOrDateTime _ (AllDay day)      = dayToText day
formatDateOrDateTime tz (AtPoint utcTime) = localTimeToText (utcTimeAtTz tz utcTime)

textShow :: (Show b, IsString a) => b -> a
textShow = fromString . show

timeOfDayToText :: TimeOfDay -> Text.Text
timeOfDayToText (TimeOfDay hour minute _) =
  let minuteText = textShow minute
      hourText = textShow hour
      minutePadded = if minute < 10 then "0" <> minuteText else minuteText
  in hourText <> ":" <> minutePadded <> " Uhr"

formatTime :: TZ -> AppointedTime -> Text.Text
formatTime tz (OnlyStart dateOrDt) = formatDateOrDateTime tz dateOrDt
formatTime tz (Range start end)    = "vom " <> formatDateOrDateTime tz start <> " bis " <> formatDateOrDateTime tz end

formatEventAsText :: TZ -> Appt TimeOrRepeat -> Text.Text
formatEventAsText tz e =
  case apptTime e of
    Time t    -> apptSummary e <> " " <> formatTime tz t
    Repeat ri -> apptSummary e <> " " <> formatTime tz (riFirstRep ri)

formatDiffAsText :: TZ -> EventDifference -> Text.Text
formatDiffAsText tz (DiffNew e)      = "Neuer Termin: " <> formatEventAsText tz e
formatDiffAsText tz (DiffDeleted e)  = "Termin entfernt: " <> formatEventAsText tz e
formatDiffAsText tz (DiffModified e) = "Termin anders: " <> formatEventAsText tz e

formatDiffs :: TZ -> [EventDifference] -> Maybe MatrixMessage
formatDiffs _ [] = Nothing
formatDiffs tz xs = Just (plainMessage (Text.intercalate "," (formatDiffAsText tz <$> xs)))

-- |Format the appointment as if it's happening now.
formatSubApptNow :: SubAppt -> MatrixMessage
formatSubApptNow sa = plainMessage $ case saStatus sa of
  ApptStarts -> "Beginnt jetzt: " <> apptSummary (saAppt sa)
  ApptEnds   -> "Endet jetzt: " <> apptSummary (saAppt sa)

-- formatEvents :: [Appt] -> Maybe MatrixMessage
-- formatEvents [] = Nothing
-- formatEvents xs = Just (plainMessage (Text.intercalate "," (formatEventAsText tz <$> xs)))

-- |Format an appointment
formatApptShort :: TZ -> SubAppt -> Text.Text
formatApptShort tz a =
  let appt = saAppt a
  in
    case saStatus a of
      ApptStarts ->
        if appTimeStartAllDay (apptTime appt)
        then apptSummary appt <> " (ganztaegig)"
        else timeOfDayToText (apptTimeOfDayStartAtZone tz appt) <> " " <> apptSummary appt
      ApptEnds ->
        if appTimeEndAllDay (apptTime appt)
        then apptSummary appt <> " endet"
        else timeOfDayToText (fromJust (apptTimeOfDayEndAtZone tz appt)) <> " " <> apptSummary appt <> " endet"

formatTomorrowAppts :: TZ -> [SubAppt] -> MatrixMessage
formatTomorrowAppts tz appts =
  case appts of
    [x] -> plainMessage ("Termin morgen: " <> formatApptShort tz x)
    xs -> plainMessage ("Mehrere Termine morgen: " <> Text.intercalate ", " (formatApptShort tz <$> xs))

formatTodayAppts :: TZ -> [SubAppt] -> MatrixMessage
formatTodayAppts tz appts =
  case appts of
    [x] -> plainMessage ("Termin heute: " <> formatApptShort tz x)
    xs -> plainMessage ("Mehrere Termine heute: " <> Text.intercalate ", " (formatApptShort tz <$> xs))
