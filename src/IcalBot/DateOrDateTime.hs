module IcalBot.DateOrDateTime(
    DateOrDateTime(..)
  , dateTimeFromIcal
  , dayForDateTime
  , timeForDateTime
  , addDuration
  , timeForDateTimeOrMidnight
  , isAllDay
  ) where

import           Control.Applicative  (pure)
import           Control.Lens         (from, view, (^.))
import           Data.AffineSpace     ((.+^))
import           Data.Bool            (Bool (False, True))
import           Data.Eq              (Eq)
import           Data.Maybe           (Maybe (Just, Nothing))
import           Data.String          (String)
import qualified Data.Text.Lazy       as LazyText
import           Data.Thyme.Calendar  (Day)
import           Data.Thyme.Clock     (NominalDiffTime, UTCTime, _utctDay)
import           Data.Thyme.LocalTime (LocalTime (..), TimeZone, midnight, utc,
                                       utcLocalTime)
import           Data.Thyme.Time.Core (fromThyme, toThyme)
import qualified Data.Time.Clock      as TimeClock
import           Data.Time.Zones      (loadTZFromDB, timeZoneForUTCTime)
import           IcalBot.Util         (localTimeToUTCTime)
import           System.IO            (IO)
import           Text.ICalendar.Types (DateTime (FloatingDateTime, UTCDateTime, ZonedDateTime))
import           Text.Show            (Show)

-- |Either a specific day, or a specific point in time
data DateOrDateTime = AllDay Day
                    | AtPoint UTCTime
                    deriving(Show, Eq)

addDuration :: DateOrDateTime -> NominalDiffTime -> DateOrDateTime
addDuration x d = AtPoint (timeForDateTimeOrMidnight x .+^ d)

isAllDay :: DateOrDateTime -> Bool
isAllDay (AllDay _)  = True
isAllDay (AtPoint _) = False

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


dayForDateTime :: DateOrDateTime -> Day
dayForDateTime (AllDay d)  = d
dayForDateTime (AtPoint d) = view _utctDay d

-- |Return the date and time if available (if it's at point) or Nothing otherwise
timeForDateTime :: DateOrDateTime -> Maybe UTCTime
timeForDateTime (AllDay _)  = Nothing
timeForDateTime (AtPoint d) = Just d

timeForDateTimeOrMidnight :: DateOrDateTime -> UTCTime
timeForDateTimeOrMidnight (AllDay d)  = localTimeToUTCTime (LocalTime d midnight)
timeForDateTimeOrMidnight (AtPoint d) = d
