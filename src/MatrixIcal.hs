{-# LANGUAGE OverloadedStrings #-}
module MatrixIcal where

import           Control.Applicative            (pure)
import           Control.Exception              (catch)
import           Control.Lens                   (from, view, (^.))
import           Control.Monad                  (join)
import           Data.Bool                      (Bool (..), not)
import           Data.Default                   (def)
import           Data.Either                    (Either (..))
import           Data.Eq                        (Eq, (==))
import           Data.Foldable                  (concatMap, foldMap)
import           Data.Function                  (const, flip, ($), (.))
import           Data.Functor                   ((<$>))
import           Data.Int                       (Int)
import           Data.List                      (any, filter, isInfixOf)
import           Data.Map.Lazy                  (toList)
import           Data.Maybe                     (Maybe (..), catMaybes)
import           Data.Monoid                    (Monoid, (<>))
import           Data.Ord                       (Ord, comparing, (<), (>))
import           Data.String                    (IsString (fromString), String)
import qualified Data.Text                      as StrictText
import qualified Data.Text.Lazy                 as LazyText
import           Data.Thyme.Calendar            (Day, gregorian, _ymdDay,
                                                 _ymdMonth, _ymdYear)
import           Data.Thyme.Clock               (UTCTime, getCurrentTime,
                                                 _utctDay, _utctDayTime,
                                                 _utctDayTime)
import           Data.Thyme.LocalTime           (LocalTime (..), TimeOfDay (..),
                                                 addMinutes, midnight,
                                                 timeOfDay, utc, utcLocalTime,
                                                 _localDay)
import           Data.Thyme.Time.Core           (toThyme)
import           Data.Traversable               (traverse)
import           Data.Tuple                     (fst, snd)
import           Lucid                          (Html)
import           MatrixIcalUtil                 (eitherRight, firstLeftOrRights,
                                                 listDirectory, minimumBySafe,
                                                 showException)
import           Prelude                        (pred, succ, (*))
import           System.FSNotify                (Event (..))
import           System.IO                      (FilePath, IO)
import           Text.ICalendar.Parser          (parseICalendarFile)
import           Text.ICalendar.Types           (DTEnd (..), DTStart (..),
                                                 Date (..), DateTime (..),
                                                 VCalendar, VEvent,
                                                 summaryValue, vcEvents,
                                                 veDTStart, veSummary)
import           Text.Show                      (show)
import           Web.Matrix.Bot.IncomingMessage (IncomingMessage,
                                                 constructIncomingMessage)

parseICalendarFileSafe :: FilePath -> IO (Either String [VCalendar])
parseICalendarFileSafe fn = (fst <$>) <$> parseICalendarFile def fn `catch` (pure . Left . showException)

veventsInCals :: [VCalendar] -> [VEvent]
veventsInCals = concatMap ((snd <$>) . toList . vcEvents)

processIcalFile :: FilePath -> (IncomingMessage StrictText.Text (Html ()) -> IO ()) -> StrictText.Text -> IO ()
processIcalFile fn mySendMessage mode = do
  result <- parseICalendarFileSafe fn
  case result of
    Left e ->
      let errorMessage = "error reading ical file “" <> fromString fn <> "”: " <> fromString e
      in mySendMessage (constructIncomingMessage errorMessage Nothing)
    Right vcals ->
      let
          appendMode :: StrictText.Text -> StrictText.Text
          appendMode x = mode <> ": " <> x
          simpleMessage :: StrictText.Text -> IncomingMessage StrictText.Text (Html ())
          simpleMessage = flip constructIncomingMessage Nothing
          forEvent :: VEvent -> IO ()
          forEvent = mySendMessage . simpleMessage . appendMode . LazyText.toStrict . formatEvent
      in foldMap forEvent (veventsInCals vcals)

eventPath :: Event -> FilePath
eventPath (Added fn _)    = fn
eventPath (Modified fn _) = fn
eventPath (Removed fn _)  = fn

validEventPath :: FilePath -> Bool
validEventPath e = not (any (`isInfixOf` e) [".Radicale.cache",".Radicale.tmp"])

newtype Seconds = Seconds { getSeconds :: Int } deriving(Eq,Ord)

data DaySituation =
    SituationWayBefore
  | SituationAfter
  | SituationDayBeforeBeforeThreshold
  | SituationDayBeforeAfterThreshold
  | SituationOnDayBeforeThreshold
  | SituationOnDayAfterThreshold

beforeEveningThreshold :: TimeOfDay
beforeEveningThreshold = snd ((21*60) `addMinutes` midnight)

beforeMorningThreshold :: TimeOfDay
beforeMorningThreshold = snd ((9*60) `addMinutes` midnight)

localTimeToUTCTime :: LocalTime -> UTCTime
localTimeToUTCTime = view (from (utcLocalTime utc))

dayToStart :: UTCTime -> Day -> Maybe UTCTime
dayToStart now tday =
  let currentDay = now ^. _utctDay
      currentTime = now ^. _utctDayTime . timeOfDay
      utcTime day tod = localTimeToUTCTime (LocalTime day tod)
      dayBeforeThreshold = utcTime (pred tday) beforeEveningThreshold
      onDayThreshold = utcTime tday beforeMorningThreshold
  in
    if currentDay == tday
    then if currentTime < beforeMorningThreshold
         then Just onDayThreshold
         else Nothing
    else if succ currentDay == tday
         then if currentTime < beforeEveningThreshold
              then Just dayBeforeThreshold
              else Just onDayThreshold
         else if currentDay > tday
              then Nothing
              else Just dayBeforeThreshold

translateStart :: UTCTime -> DTStart -> Maybe UTCTime
translateStart now (DTStartDate (Date day) _) = dayToStart now (toThyme day)
translateStart now (DTStartDateTime (FloatingDateTime localTime) _) = dayToStart now (toThyme localTime ^. _localDay)
translateStart now (DTStartDateTime (UTCDateTime utcTime) _) = dayToStart now (toThyme utcTime ^. _utctDay)
translateStart now (DTStartDateTime (ZonedDateTime localTime _) _) = dayToStart now (toThyme localTime ^. _localDay)

latestEvent' :: UTCTime -> [VCalendar] -> Maybe (UTCTime,LazyText.Text)
latestEvent' now vcals =
  let
      processVEvent :: VEvent -> Maybe (UTCTime,LazyText.Text)
      processVEvent e = do
        start <- veDTStart e
        timerTime <- translateStart now start
        pure (timerTime,formatEvent e)
  in minimumBySafe (comparing fst) (catMaybes (processVEvent <$> veventsInCals vcals))

latestEvent :: FilePath -> IO (Maybe (UTCTime,LazyText.Text))
latestEvent icalDir = do
  files <- filter validEventPath <$> listDirectory icalDir
  eithers <- traverse parseICalendarFileSafe files
  now <- getCurrentTime
  pure $ eitherRight (join <$> firstLeftOrRights eithers) (const Nothing) (latestEvent' now)

dayToText :: IsString a => Day -> a
dayToText day =
  let ymd = day ^. gregorian
  in fromString $ " am " <> show (ymd ^. _ymdDay) <> "." <> show (ymd ^. _ymdMonth) <> "." <> show (ymd ^. _ymdYear)

textShow :: IsString a => Int -> a
textShow = fromString . show

timeOfDayToText :: (IsString a,Monoid a) => TimeOfDay -> a
timeOfDayToText (TimeOfDay hour minute _) =
  let minuteText = textShow minute
      hourText = textShow hour
      minutePadded = if minute < 10 then "0" <> minuteText else minuteText
  in hourText <> ":" <> minutePadded <> " Uhr"

localTimeToText :: (IsString a,Monoid a) => LocalTime -> a
localTimeToText (LocalTime day tod) =
  let dayText = dayToText day
      timeOfDayText = timeOfDayToText tod
  in dayText <> " " <> timeOfDayText

utcTimeToText :: (IsString a,Monoid a) => UTCTime -> a
utcTimeToText t =
  let dayText = dayToText (t ^. _utctDay)
      dayTimeInt = t ^. _utctDayTime . timeOfDay
      dt = timeOfDayToText dayTimeInt
  in dayText <> " " <> dt

startDateToText :: DTStart -> LazyText.Text
startDateToText (DTStartDateTime (FloatingDateTime localTime) _) = localTimeToText (toThyme localTime)
startDateToText (DTStartDateTime (UTCDateTime utcTime) _) = utcTimeToText (toThyme utcTime)
startDateToText (DTStartDateTime (ZonedDateTime localTime timeZone) _) = localTimeToText (toThyme localTime) <> "@" <> timeZone
startDateToText (DTStartDate (Date day) _) = dayToText (toThyme day)

endDateToText :: DTEnd -> LazyText.Text
endDateToText (DTEndDateTime (FloatingDateTime localTime) _) = localTimeToText (toThyme localTime)
endDateToText (DTEndDateTime (UTCDateTime utcTime) _) = utcTimeToText (toThyme utcTime)
endDateToText (DTEndDateTime (ZonedDateTime localTime timeZone) _) = localTimeToText (toThyme localTime) <> "@" <> timeZone
endDateToText (DTEndDate (Date day) _) = dayToText (toThyme day)

formatEvent :: VEvent -> LazyText.Text
formatEvent e =
  case veSummary e of
    Nothing -> "event has no summary, cannot generate textual representation"
    Just summary' ->
      let summary = summaryValue summary'
      in case veDTStart e of
           Nothing        -> summary
           Just startDate -> summary <> startDateToText startDate
