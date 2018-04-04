{-# LANGUAGE TemplateHaskell #-}
import           Control.Applicative            (pure)
import           Data.AdditiveGroup             (zeroV)
import           Data.String                    (String)
import qualified Data.Text                      as Text
import           Data.Thyme.Calendar            (Day)
import           Data.Thyme.Clock               (UTCTime)
import           Data.Thyme.Format              (readTime)
import           Data.Thyme.LocalTime           (TimeOfDay (..))
import           IcalBot.Appointment            (AppointedTime (..),
                                                 Appointment (..),
                                                 DateOrDateTime (..))
import           IcalBot.EventDB                (EventDifference (..),
                                                 compareDB, eventDBFromFile,
                                                 eventDBFromList)
import           IcalBot.Formatting             (dayToText,
                                                 formatDateOrDateTime,
                                                 formatTime, timeOfDayToText,
                                                 utcTimeToText)
import           System.IO                      (IO)
import           System.Locale                  (defaultTimeLocale)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.TH              (defaultMainGenerator)
import           Test.HUnit                     ((@?=))

readTimeString :: String -> UTCTime
readTimeString = readTime defaultTimeLocale "%FT%T%Q"

readDayString :: String -> Day
readDayString = readTime defaultTimeLocale "%F"

case_eventWithRange = do
  let fn = "test/data/created_in_thunderbird.ical"
  appt <- eventDBFromFile fn
  let start = readTimeString "2017-11-01T10:00:00"
      end = readTimeString "2017-11-01T11:00:00"
  let testAppt = Appointment {
          _iePath = fn
        , _ieSummary = "testevent"
        , _ieTime = Range (AtPoint start) (AtPoint end)
        , _ieUid = "06c41895-6d2c-44c2-ae29-50fa85692765"
        }
  appt @?= (eventDBFromList [testAppt])

case_eventWithDate = do
  let fn = "test/data/just_date.ical"
  appt <- eventDBFromFile fn
  let testAppt = Appointment {
          _iePath = fn
        , _ieSummary = "dateevent"
        , _ieTime = Range (AllDay (readDayString "2019-05-31")) (AllDay (readDayString "2019-06-03"))
        , _ieUid = "f8a596b2-2c5c-4628-831a-1d505da9ae18"
        }
  appt @?= (eventDBFromList [testAppt])

dummyAppt :: Text.Text -> Appointment
dummyAppt uid = dummyApptSummary uid uid

dummyApptSummary :: Text.Text -> Text.Text -> Appointment
dummyApptSummary uid summary =
  Appointment {
    _iePath = "/some/path.ical"
  , _ieSummary = summary
  , _ieTime = Range (AllDay (readDayString "2019-05-31")) (AllDay (readDayString "2019-06-03"))
  , _ieUid = uid
  }

case_compareDbNew = do
  let oldItem = dummyAppt "foo"
      newItem = dummyAppt "bar"
      oldDb = eventDBFromList [oldItem]
      newDb = eventDBFromList [oldItem, newItem]
      comparison = compareDB oldDb newDb
  comparison @?= [DiffNew newItem]

case_compareDbDeleted = do
  let oldItem = dummyAppt "foo"
      newItem = dummyAppt "bar"
      oldDb = eventDBFromList [oldItem, newItem]
      newDb = eventDBFromList [oldItem]
      comparison = compareDB oldDb newDb
  comparison @?= [DiffDeleted newItem]

case_compareDbModified = do
  let oldItem = dummyAppt "foo"
      newItem = dummyApptSummary "bar" "firstsummary"
      newItem' = dummyApptSummary "bar" "othersummary"
      oldDb = eventDBFromList [oldItem, newItem]
      newDb = eventDBFromList [oldItem, newItem']
      comparison = compareDB oldDb newDb
  comparison @?= [DiffModified newItem']

case_timeOfDayToText =
  let tod = TimeOfDay 16 02 zeroV
  in timeOfDayToText tod @?= "16:02 Uhr"

case_utcTimeToText = utcTimeToText (readTimeString "2018-10-02T16:01:00") @?= "2.10.2018 16:01 Uhr"

case_dayToText = dayToText (readDayString "2018-10-02") @?= "2.10.2018"

case_formatDateOrDateTimeAllDay =
  formatDateOrDateTime (AllDay (readDayString "2018-10-02")) @?= "2.10.2018"

case_formatDateOrDateTimeAtPoint =
  formatDateOrDateTime (AtPoint (readTimeString "2018-10-02T16:01:00")) @?= "2.10.2018 16:01 Uhr"

case_formatTimeOnlyStart = formatTime (OnlyStart (AllDay (readDayString "2018-10-02"))) @?= "2.10.2018"

case_formatTimeRange = formatTime (Range (AllDay (readDayString "2018-10-02")) (AllDay (readDayString "2018-10-03"))) @?= "Vom 2.10.2018 bis 3.10.2018"

-- case_formatEventAsText = do
--   undefined

main :: IO ()
main = $(defaultMainGenerator)

{-
import           Data.Function       (($))
import           Data.Functor        ((<$>))
import           Data.Maybe          (Maybe (Just, Nothing))
import           Data.String         (String)
import           Data.Thyme.Calendar (Day)
import           Data.Thyme.Clock    (UTCTime)
import           Data.Thyme.Format   (formatTime, readTime)
import           MatrixIcal          (dayToStart)
import           Prelude             (undefined)
import           System.IO           (IO)
import           System.Locale       (defaultTimeLocale)
import           Test.Tasty          (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit    (testCase, (@?=))

main :: IO ()
main = defaultMain unitTests

timeFormat :: String
timeFormat = "%FT%H:%M:%S"

utcTimeToString :: UTCTime -> String
utcTimeToString = formatTime defaultTimeLocale timeFormat

stringToUtcTime :: String -> UTCTime
stringToUtcTime = readTime defaultTimeLocale timeFormat

dayFormat :: String
dayFormat = "%F"

stringToDay :: String -> Day
stringToDay = readTime defaultTimeLocale dayFormat

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [dayToStartInPast,dayToStartTodayBeforeThreshold,dayToStartTodayAfterThreshold,dayToStartYesterdayBeforeThreshold,dayToStartYesterdayAfterThreshold,dayToStartWayBefore]

dayToStartString :: String -> String -> Maybe String
dayToStartString now day = utcTimeToString <$> dayToStart (stringToUtcTime now) (stringToDay day)

dayToStartTodayBeforeThreshold :: TestTree
dayToStartTodayBeforeThreshold =
  testCase "date is today, but before threshold" $
    dayToStartString "1987-08-21T02:00:00" "1987-08-21" @?= Just "1987-08-21T09:00:00"

dayToStartTodayAfterThreshold :: TestTree
dayToStartTodayAfterThreshold =
  testCase "date is today, but after threshold" $
    dayToStartString "1987-08-21T12:00:00" "1987-08-21" @?= Nothing

dayToStartWayBefore :: TestTree
dayToStartWayBefore =
  testCase "appointment is way in the future" $
    dayToStartString "1987-08-21T12:00:00" "1987-08-23" @?= Just "1987-08-22T21:00:00"

dayToStartYesterdayBeforeThreshold :: TestTree
dayToStartYesterdayBeforeThreshold =
  testCase "date is one day before appointment and before evening threshold" $
    dayToStartString "1987-08-20T12:00:00" "1987-08-21" @?= Just "1987-08-20T21:00:00"

dayToStartYesterdayAfterThreshold :: TestTree
dayToStartYesterdayAfterThreshold =
  testCase "date is one day before appointment and after evening threshold" $
    dayToStartString "1987-08-20T22:00:00" "1987-08-21" @?= Just "1987-08-21T09:00:00"

dayToStartInPast :: TestTree
dayToStartInPast =
  testCase "date way in the future should return" $
    dayToStartString "1987-08-21T00:00:00" "1987-08-19" @?= Nothing
-}
