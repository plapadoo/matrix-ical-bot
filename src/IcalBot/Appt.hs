{-# LANGUAGE DeriveFunctor #-}
-- |Functions for the appointment data structure and its friends
-- (mostly wrapping Icalendar types)
module IcalBot.Appt(
    Appt(..)
  , apptStartDay
  , apptEndDay
  , fromIcal
  , fromIcalNoRepeats
  , firstRep
  , apptTimeOfDayStart
  , apptTimeOfDayStartAtZone
  , apptTimeOfDayEnd
  , apptTimeOfDayEndAtZone
  ) where

import           Control.Applicative                          (pure)
import           Control.Lens                                 (view)
import           Control.Monad.Loops                          (dropWhileM)
import           Data.Bool                                    (Bool (True))
import           Data.Either                                  (Either (Left, Right))
import           Data.Eq                                      (Eq)
import           Data.Foldable                                (toList)
import           Data.Function                                (($), (.))
import           Data.Functor                                 (Functor, (<$>))
import           Data.Maybe                                   (Maybe (Just, Nothing),
                                                               listToMaybe)
import           Data.Ord                                     ((<))
import qualified Data.Text                                    as Text
import qualified Data.Text.Lazy                               as LazyText
import           Data.Thyme.Calendar                          (Day)
import           Data.Thyme.Clock                             (TimeDiff,
                                                               UTCTime,
                                                               fromSeconds,
                                                               _utctDayTime)
import           Data.Thyme.LocalTime                         (TimeOfDay,
                                                               timeOfDay)
import           Data.Thyme.Time.Core                         (toThyme)
import           Data.Time.Zones                              (TZ)
import           IcalBot.AppointedTime                        (AppointedTime (OnlyStart, Range),
                                                               appTimeEnd,
                                                               appTimeEndUtc,
                                                               appTimeStart,
                                                               appTimeStartUtc)
import           IcalBot.DateOrDateTime                       (DateOrDateTime (AllDay),
                                                               addDuration,
                                                               dateTimeFromIcal,
                                                               dayForDateTime)
import           IcalBot.RepeatInfo                           (RepeatInfo (..))
import           IcalBot.TimeOrRepeat                         (TimeOrRepeat (Repeat, Time))
import           IcalBot.Util                                 (timeOfDayAtTz)
import           Prelude                                      (Num, (*), (+))
import           System.FilePath                              (FilePath)
import           System.IO                                    (IO)
import           Text.ICalendar.Recurrence.ByRules            (rRuleToRRuleFunc)
import           Text.ICalendar.Types.Components              (VEvent (veDTEndDuration, veDTStart, veRRule, veSummary, veUID))
import           Text.ICalendar.Types.Properties.DateTime     (DTEnd (dtEndValue),
                                                               DTStart (dtStartValue),
                                                               DurationProp (..),
                                                               VDateTime (VDate, VDateTime))
import           Text.ICalendar.Types.Properties.Descriptive  (Summary (summaryValue))
import           Text.ICalendar.Types.Properties.Relationship (UID (uidValue))
import           Text.ICalendar.Types.Values                  (Date (..), Duration (DurationDate, DurationTime, DurationWeek),
                                                               Sign (Negative, Positive))
import           Text.Show                                    (Show)


-- |An event in the internal format (using "our" data types instead of
-- the one icalendar provides; this makes lots of stuff easier, like
-- using strict texts everywhere, or boiling down the overengineered
-- date formats in ical). Here, we are deliberately ignoring other
-- types of "stuff" in ical, like "free busys", "journals", "todos",
-- ...
data Appt a = Appt {
  -- |Path of the file from where the event originated from (this
  -- assumes that stuff comes from files, only)
    apptPath    :: FilePath
  -- |Summary, taken directly from ical
  , apptSummary :: Text.Text
  -- |Boiled down time value in our internal format.
  , apptTime    :: a
  -- |UID without the "other" parameter from ical
  , apptUid     :: Text.Text
  } deriving(Show, Eq, Functor)

apptStartDay :: Appt AppointedTime -> Day
apptStartDay = dayForDateTime . appTimeStart . apptTime

apptEndDay :: Appt AppointedTime -> Maybe Day
apptEndDay = (dayForDateTime <$>) . appTimeEnd . apptTime

apptTimeOfDayStartAtZone :: TZ -> Appt AppointedTime -> TimeOfDay
apptTimeOfDayStartAtZone tz = timeOfDayAtTz tz . appTimeStartUtc . apptTime

apptTimeOfDayEndAtZone :: TZ -> Appt AppointedTime -> Maybe TimeOfDay
apptTimeOfDayEndAtZone tz = (timeOfDayAtTz tz <$>) . appTimeEndUtc . apptTime

apptTimeOfDayStart :: Appt AppointedTime -> TimeOfDay
apptTimeOfDayStart = view timeOfDay . view _utctDayTime . appTimeStartUtc . apptTime

apptTimeOfDayEnd :: Appt AppointedTime -> Maybe TimeOfDay
apptTimeOfDayEnd = (view timeOfDay <$>) . (view _utctDayTime <$>) . appTimeEndUtc . apptTime

signToNumber :: Num a => Sign -> a
signToNumber Positive = 1
signToNumber Negative = -1

-- This is deliberately missing leap seconds, time shifts etc.
durationToDiffTime :: TimeDiff d => Duration -> d
durationToDiffTime (DurationDate sign day hour minute second) = fromSeconds (signToNumber sign * (second + 60 * minute + 60 * 60 * hour + 86400 * day))
durationToDiffTime (DurationTime sign hour minute second)     = fromSeconds (signToNumber sign * (second + 60 * minute + 60 * 60 * hour))
durationToDiffTime (DurationWeek sign week)                   = fromSeconds (signToNumber sign * 86400 * 7 * week)

-- |Convert a start and an optional end (or a duration) to the internal format
timeFromIcal :: DTStart -> Maybe (Either DTEnd DurationProp) -> IO AppointedTime
timeFromIcal start end = do
  start' <-
    case dtStartValue start of
      VDate (Date d) -> pure (AllDay (toThyme d))
      VDateTime dt   -> dateTimeFromIcal dt
  case end of
    Nothing ->
      pure (OnlyStart start')
    Just end' ->
      case end' of
        Left x -> case dtEndValue x of
            VDate (Date d) -> pure (Range start' (AllDay (toThyme d)))
            VDateTime d    -> Range start' <$> dateTimeFromIcal d
        -- If it's one day duration and the start is just a date, then
        -- it's an all-day in disguise and we have no end
        Right (DurationProp d@(DurationDate Positive 1 0 0 0) _)    ->
          case start' of
            AllDay _ -> pure (OnlyStart start')
            _ -> pure (Range start' (addDuration start' (durationToDiffTime d)))
        Right (DurationProp d _)    -> pure (Range start' (addDuration start' (durationToDiffTime d)))

-- |Return the first repetition of an event after the specified date
firstRep :: UTCTime -> [VEvent] -> IO (Maybe VEvent)
firstRep after es = listToMaybe <$> dropWhileM eventPassed es
  where eventPassed :: VEvent -> IO Bool
        eventPassed ev = case veDTStart ev of
          Nothing -> pure True
          Just start -> do
            time <- startUtc start
            pure (time < after)
        startUtc :: DTStart -> IO UTCTime
        startUtc start = appTimeStartUtc <$> timeFromIcal start Nothing

-- FIXME: Redundancy
fromIcalNoRepeats :: FilePath -> VEvent -> IO (Maybe (Appt AppointedTime))
fromIcalNoRepeats fn e =
  case veDTStart e of
    Nothing -> pure Nothing
    Just start ->
      case veSummary e of
        Nothing -> pure Nothing
        Just summary -> do
          t <- timeFromIcal start (veDTEndDuration e)
          pure $ Just Appt {
            apptPath = fn
            , apptSummary = (LazyText.toStrict . summaryValue) summary
            , apptTime = t
            , apptUid = (LazyText.toStrict . uidValue . veUID) e
            }

-- |Convert an event from a file to the internal format. This might
-- return Nothing in case the format isn't processable later on (for
-- example, if we have no start date)
fromIcal :: FilePath -> VEvent -> IO (Maybe (Appt TimeOrRepeat))
fromIcal fn e =
  case veDTStart e of
    Nothing -> pure Nothing
    Just start ->
      case veSummary e of
        Nothing -> pure Nothing
        Just summary ->
          case toList (veRRule e) of
            [] -> do
              t <- timeFromIcal start (veDTEndDuration e)
              pure $ Just Appt {
                apptPath = fn
                , apptSummary = (LazyText.toStrict . summaryValue) summary
                , apptTime = Time t
                , apptUid = (LazyText.toStrict . uidValue . veUID) e
                }
            (r:_) -> do
              first <- timeFromIcal start (veDTEndDuration e)
              pure $ Just Appt {
                apptPath = fn
                , apptSummary = (LazyText.toStrict . summaryValue) summary
                , apptTime = Repeat (RepeatInfo (rRuleToRRuleFunc r e) first)
                , apptUid = (LazyText.toStrict . uidValue . veUID) e
                }
