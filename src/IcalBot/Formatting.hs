-- |Functions to format the internal data structures and date/times
module IcalBot.Formatting(
    dayToText
  , utcTimeToText
  , formatDateOrDateTime
  , timeOfDayToText
  , formatTime
  , formatEventAsText
  , formatDiffAsText
  , formatDiffs
  , formatEvents
  ) where

import           Control.Lens          ((^.))
import           Data.Function         (($), (.))
import           Data.Functor          ((<$>))
import           Data.Int              (Int)
import           Data.Maybe            (Maybe (Just, Nothing))
import           Data.Monoid           ((<>))
import           Data.Ord              ((<))
import           Data.String           (IsString (fromString))
import qualified Data.Text             as Text
import           Data.Thyme.Calendar   (Day, gregorian, _ymdDay, _ymdMonth,
                                        _ymdYear)
import           Data.Thyme.Clock      (UTCTime, _utctDay, _utctDayTime)
import           Data.Thyme.LocalTime  (TimeOfDay (..), timeOfDay)
import           IcalBot.Appointment   (AppointedTime (OnlyStart, Range),
                                        Appointment,
                                        DateOrDateTime (AllDay, AtPoint),
                                        ieSummary, ieTime)
import           IcalBot.EventDB       (EventDifference (..))
import           IcalBot.MatrixMessage (MatrixMessage, plainMessage)
import           Text.Show             (show)

dayToText :: Day -> Text.Text
dayToText day =
  let ymd = day ^. gregorian
  in Text.pack $ show (ymd ^. _ymdDay) <> "." <> show (ymd ^. _ymdMonth) <> "." <> show (ymd ^. _ymdYear)

utcTimeToText :: UTCTime -> Text.Text
utcTimeToText t =
  let dayText :: Text.Text
      dayText = dayToText (t ^. _utctDay)
      dayTimeInt = t ^. _utctDayTime . timeOfDay
      dt :: Text.Text
      dt = timeOfDayToText dayTimeInt
  in dayText <> " " <> dt

formatDateOrDateTime :: DateOrDateTime -> Text.Text
formatDateOrDateTime (AllDay day)      = dayToText day
formatDateOrDateTime (AtPoint utcTime) = utcTimeToText utcTime

textShow :: IsString a => Int -> a
textShow = fromString . show

timeOfDayToText :: TimeOfDay -> Text.Text
timeOfDayToText (TimeOfDay hour minute _) =
  let minuteText = textShow minute
      hourText = textShow hour
      minutePadded = if minute < 10 then "0" <> minuteText else minuteText
  in hourText <> ":" <> minutePadded <> " Uhr"

formatTime :: AppointedTime -> Text.Text
formatTime (OnlyStart dateOrDt) = formatDateOrDateTime dateOrDt
formatTime (Range start end)    = "Vom " <> formatDateOrDateTime start <> " bis " <> formatDateOrDateTime end

formatEventAsText :: Appointment -> Text.Text
formatEventAsText e = (e ^. ieSummary) <> " " <> formatTime (e ^. ieTime)

formatDiffAsText :: EventDifference -> Text.Text
formatDiffAsText (DiffNew e)      = "Neuer Termin: " <> formatEventAsText e
formatDiffAsText (DiffDeleted e)  = "Termin entfernt: " <> formatEventAsText e
formatDiffAsText (DiffModified e) = "Termin anders: " <> formatEventAsText e

formatDiffs :: [EventDifference] -> Maybe MatrixMessage
formatDiffs [] = Nothing
formatDiffs xs = Just (plainMessage (Text.intercalate "," (formatDiffAsText <$> xs)))

formatEvents :: [Appointment] -> Maybe MatrixMessage
formatEvents [] = Nothing
formatEvents xs = Just (plainMessage (Text.intercalate "," (formatEventAsText <$> xs)))
