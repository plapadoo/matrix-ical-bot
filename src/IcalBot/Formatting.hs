-- |Functions to format the internal data structures and date/times
module IcalBot.Formatting(
    dayToText
  , localTimeToText
  , formatDateOrDateTime
  , timeOfDayToText
  , formatTime
  , formatEventAsText
  , formatDiffAsText
  , formatDiffs
  , textShow
  ) where

import           Control.Lens          ((^.))
import           Data.Function         (($), (.))
import           Data.Functor          ((<$>))
import           Data.Maybe            (Maybe (Just, Nothing))
import           Data.Monoid           ((<>))
import           Data.Ord              ((<))
import           Data.String           (IsString (fromString))
import qualified Data.Text             as Text
import           Data.Thyme.Calendar   (Day, gregorian, _ymdDay, _ymdMonth,
                                        _ymdYear)
import           Data.Thyme.LocalTime  (LocalTime, TimeOfDay (..), _localDay,
                                        _localTimeOfDay)
import           Data.Time.Zones       (TZ)
import           IcalBot.Appointment   (AppointedTime (OnlyStart, Range),
                                        Appointment,
                                        DateOrDateTime (AllDay, AtPoint),
                                        ieSummary, ieTime)
import           IcalBot.EventDB       (EventDifference (..))
import           IcalBot.MatrixMessage (MatrixMessage, plainMessage)
import           IcalBot.Util          (utcTimeAtTz)
import           Text.Show             (Show, show)

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

formatEventAsText :: TZ -> Appointment -> Text.Text
formatEventAsText tz e = ieSummary e <> " " <> formatTime tz (ieTime e)

formatDiffAsText :: TZ -> EventDifference -> Text.Text
formatDiffAsText tz (DiffNew e)      = "Neuer Termin: " <> formatEventAsText tz e
formatDiffAsText tz (DiffDeleted e)  = "Termin entfernt: " <> formatEventAsText tz e
formatDiffAsText tz (DiffModified e) = "Termin anders: " <> formatEventAsText tz e

formatDiffs :: TZ -> [EventDifference] -> Maybe MatrixMessage
formatDiffs _ [] = Nothing
formatDiffs tz xs = Just (plainMessage (Text.intercalate "," (formatDiffAsText tz <$> xs)))

-- formatEvents :: [Appointment] -> Maybe MatrixMessage
-- formatEvents [] = Nothing
-- formatEvents xs = Just (plainMessage (Text.intercalate "," (formatEventAsText tz <$> xs)))
