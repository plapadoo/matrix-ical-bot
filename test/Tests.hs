{-# LANGUAGE TemplateHaskell #-}
import           Control.Applicative            (pure)
import           Data.AdditiveGroup             (zeroV)
import           Data.Bifunctor                 (second)
import           Data.Functor                   ((<$>))
import           Data.String                    (String)
import qualified Data.Text                      as Text
import           Data.Thyme.Calendar            (Day)
import           Data.Thyme.Clock               (UTCTime)
import           Data.Thyme.Format              (readTime)
import           Data.Thyme.LocalTime           (TimeOfDay (..), utc)
import           Data.Time.Zones                (loadTZFromDB, utcTZ)
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
import           IcalBot.MatrixMessage          (messagePlainText, plainMessage)
import           IcalBot.Scheduling             (collectAppointments)
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

case_collectAppointsmentsOnlyStartAtPoint = do
  let start = readTimeString "2017-11-01T10:00:00"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appointment {
          _iePath = "/some/path.ical"
        , _ieSummary = "foo"
        , _ieTime = OnlyStart (AtPoint start)
        , _ieUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppointments db utcTZ now)
  result @?= [ (readTimeString "2017-11-01T10:00:00", "Beginnt jetzt: foo")
             , (readTimeString "2017-10-31T20:00:00", "Termin morgen: 10:00 Uhr foo")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: 10:00 Uhr foo")
             ]

case_collectAppointsmentsOnlyStartAtPointEurope = do
  tz <- loadTZFromDB "Europe/Berlin"
  let start = readTimeString "2017-11-01T10:00:00"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appointment {
          _iePath = "/some/path.ical"
        , _ieSummary = "foo"
        , _ieTime = OnlyStart (AtPoint start)
        , _ieUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppointments db tz now)
  result @?= [ (readTimeString "2017-11-01T10:00:00", "Beginnt jetzt: foo")
             , (readTimeString "2017-10-31T20:00:00", "Termin morgen: 11:00 Uhr foo")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: 11:00 Uhr foo")
             ]

case_collectAppointsmentsOnlyStartAllDay = do
  let start = readDayString "2017-11-01"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appointment {
          _iePath = "/some/path.ical"
        , _ieSummary = "foo"
        , _ieTime = OnlyStart (AllDay start)
        , _ieUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppointments db utcTZ now)
  result @?= [ (readTimeString "2017-10-31T20:00:00", "Termin morgen: foo (ganztägig)")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: foo (ganztägig)")
             ]

case_collectAppointsmentsRange = do
  let start = readTimeString "2017-11-01T10:00:00"
      end = readTimeString "2017-11-02T11:00:00"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appointment {
          _iePath = "/some/path.ical"
        , _ieSummary = "foo"
        , _ieTime = Range (AtPoint start) (AtPoint end)
        , _ieUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppointments db utcTZ now)
  result @?= [ (readTimeString "2017-11-01T10:00:00", "Beginnt jetzt: foo")
             , (readTimeString "2017-11-02T11:00:00", "Endet jetzt: foo")
             , (readTimeString "2017-10-31T20:00:00", "Termin morgen: 10:00 Uhr foo")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: 10:00 Uhr foo")
             ]


main :: IO ()
main = $(defaultMainGenerator)
