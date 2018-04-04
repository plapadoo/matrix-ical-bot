{-# LANGUAGE TemplateHaskell #-}
-- |Functions for the appointment data structure and its friends
-- (mostly wrapping Icalendar types)
module IcalBot.Appointment(
    Appointment(..)
  , iePath
  , ieUid
  , ieTime
  , fromIcal
  , DateOrDateTime(..)
  , AppointedTime(..)
  , ieSummary) where

import           Control.Applicative  (pure)
import           Control.Lens         (from, makeLenses, (^.))
import           Data.Either          (Either (Left, Right))
import           Data.Eq              (Eq)
import           Data.Function        (($), (.))
import           Data.Maybe           (Maybe (Just, Nothing))
import           Data.String          (String)
import qualified Data.Text            as Text
import qualified Data.Text.Lazy       as LazyText
import           Data.Thyme.Calendar  (Day)
import           Data.Thyme.Clock     (UTCTime)
import           Data.Thyme.LocalTime (LocalTime, TimeZone, utc, utcLocalTime)
import           Data.Thyme.Time.Core (fromThyme, toThyme)
import qualified Data.Time.Clock      as TimeClock
import           Data.Time.Zones      (loadTZFromDB, timeZoneForUTCTime)
import           Prelude              (error)
import           System.FilePath      (FilePath)
import           System.IO            (IO)
import           Text.ICalendar.Types (DTEnd (DTEndDate, DTEndDateTime),
                                       DTStart (DTStartDate, DTStartDateTime),
                                       Date (..),
                                       DateTime (FloatingDateTime, UTCDateTime, ZonedDateTime),
                                       DurationProp (..),
                                       Summary (summaryValue), UID (uidValue),
                                       VEvent (veDTEndDuration, veDTStart, veSummary, veUID))
import           Text.Show            (Show)

-- |Either a specific day, or a specific point in time
data DateOrDateTime = AllDay Day
                    | AtPoint UTCTime
                    deriving(Show, Eq)

-- |Convert a local (thyme) time stamp to UTC
localToUTC :: LocalTime -> UTCTime
localToUTC t = t ^. from (utcLocalTime utc)

-- |Convert an ical date or date/time to the internal format; this
-- method removes all time zone information present in the original
-- ical format
dateTimeFromIcal :: DateTime -> IO DateOrDateTime
dateTimeFromIcal dt =
  case dt of
    UTCDateTime d -> pure (AtPoint (toThyme d))
    ZonedDateTime local tz -> do
      let tzString :: String
          tzString = LazyText.unpack tz
      -- Technically wrong; the time zone should be parsed and
      -- interpreted from the ical file itself, but that seemed to
      -- much of a hassle as of the time of writing this.
      zoneAsTz <- loadTZFromDB tzString
      let localThyme :: LocalTime
          localThyme = toThyme local
          localAsUtc :: TimeClock.UTCTime
          localAsUtc = fromThyme (localToUTC localThyme)
          timeZone :: TimeZone
          timeZone = toThyme (timeZoneForUTCTime zoneAsTz localAsUtc)
      pure (AtPoint (localThyme ^. from (utcLocalTime timeZone)))
    -- This is actually dubious: floating date and time values
    -- are supposed to be without a time-zone, so we're changing stuff when we
    -- convert it to UTC
    FloatingDateTime d -> pure (AtPoint (localToUTC (toThyme d)))

-- |Either a point in time or a date range
data AppointedTime = OnlyStart DateOrDateTime
                  | Range DateOrDateTime DateOrDateTime
                  deriving(Show, Eq)

-- |An event in the internal format (using "our" data types instead of
-- the one icalendar provides; this makes lots of stuff easier, like
-- using strict texts everywhere, or boiling down the overengineered
-- date formats in ical). Here, we are deliberately ignoring other
-- types of "stuff" in ical, like "free busys", "journals", "todos",
-- ...
data Appointment = Appointment {
  -- |Path of the file from where the event originated from (this
  -- assumes that stuff comes from files, only)
    _iePath    :: FilePath
  -- |Summary, taken directly from ical
  , _ieSummary :: Text.Text
  -- |Boiled down time value in our internal format.
  , _ieTime    :: AppointedTime
  -- |UID without the "other" parameter from ical
  , _ieUid     :: Text.Text
  } deriving(Show, Eq)

makeLenses ''Appointment

-- |Convert a start and an optional end (or a duration) to the internal format
timeFromIcal :: DTStart -> Maybe (Either DTEnd DurationProp) -> IO AppointedTime
timeFromIcal start end = do
  start' <-
    case start of
      DTStartDate (Date d) _ -> pure (AllDay (toThyme d))
      DTStartDateTime dt _   -> dateTimeFromIcal dt
  case end of
    Nothing ->
      pure (OnlyStart start')
    Just end' ->
      case end' of
        Left (DTEndDate (Date d) _) -> pure (Range start' (AllDay (toThyme d)))
        Left (DTEndDateTime d _)    -> do
          end'' <- dateTimeFromIcal d
          pure (Range start' end'')
        Right (DurationProp _ _)              -> error "durations not supported yet"

-- |Convert an event from a file to the internal format. This might
-- return Nothing in case the format isn't processable later on (for
-- example, if we have no start date)
fromIcal :: FilePath -> VEvent -> IO (Maybe Appointment)
fromIcal fn e = case veDTStart e of
  Nothing -> pure Nothing
  Just start -> case veSummary e of
    Nothing -> pure Nothing
    Just summary -> do
      t <- timeFromIcal start (veDTEndDuration e)
      pure $ Just $ Appointment {
        _iePath = fn
        , _ieSummary = (LazyText.toStrict . summaryValue) summary
        , _ieTime = t
        , _ieUid = (LazyText.toStrict . uidValue . veUID) e
        }
