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
import           Data.Thyme.LocalTime           (LocalTime, TimeOfDay (..), utc)
import           Data.Time.Zones                (loadTZFromDB, utcTZ)
import           IcalBot.Appointment            (AppointedTime (..),
                                                 Appointment (..),
                                                 DateOrDateTime (..))
import           IcalBot.EventDB                (EventDifference (..),
                                                 compareDB, eventDBFromFile,
                                                 eventDBFromList)
import           IcalBot.Formatting             (dayToText,
                                                 formatDateOrDateTime,
                                                 formatTime, localTimeToText,
                                                 timeOfDayToText)
import           IcalBot.MatrixMessage          (messagePlainText, plainMessage)
import           IcalBot.Scheduling             (collectAppointments)
import           System.IO                      (IO)
import           System.Locale                  (defaultTimeLocale)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.TH              (defaultMainGenerator)
import           Test.HUnit                     ((@?=))

readTimeString :: String -> UTCTime
readTimeString = readTime defaultTimeLocale "%FT%T%Q"

readLocalTimeString :: String -> LocalTime
readLocalTimeString = readTime defaultTimeLocale "%FT%T%Q"

readDayString :: String -> Day
readDayString = readTime defaultTimeLocale "%F"

-- |Try to read a pre-created file and see if the data matches
case_eventWithRange = do
  let fn = "test/data/created_in_thunderbird.ical"
  appt <- eventDBFromFile fn
  let start = readTimeString "2017-11-01T10:00:00"
      end = readTimeString "2017-11-01T11:00:00"
  let testAppt = Appointment {
          iePath = fn
        , ieSummary = "testevent"
        , ieTime = Range (AtPoint start) (AtPoint end)
        , ieUid = "06c41895-6d2c-44c2-ae29-50fa85692765"
        }
  appt @?= (eventDBFromList [testAppt])

-- |Try to read a pre-created file and see if the data matches
case_eventWithDate = do
  let fn = "test/data/just_date.ical"
  appt <- eventDBFromFile fn
  let testAppt = Appointment {
          iePath = fn
        , ieSummary = "dateevent"
        , ieTime = Range (AllDay (readDayString "2019-05-31")) (AllDay (readDayString "2019-06-03"))
        , ieUid = "f8a596b2-2c5c-4628-831a-1d505da9ae18"
        }
  appt @?= (eventDBFromList [testAppt])

-- |Just an appointment with a specific UID
dummyAppt :: Text.Text -> Appointment
dummyAppt uid = dummyApptSummary uid uid

-- |Just an appointment with a specific UID and a summary
dummyApptSummary :: Text.Text -> Text.Text -> Appointment
dummyApptSummary uid summary =
  Appointment {
    iePath = "/some/path.ical"
  , ieSummary = summary
  , ieTime = Range (AllDay (readDayString "2019-05-31")) (AllDay (readDayString "2019-06-03"))
  , ieUid = uid
  }

-- |Add an element to the DB, see if it's in the "new" set
case_compareDbNew = do
  let oldItem = dummyAppt "foo"
      newItem = dummyAppt "bar"
      oldDb = eventDBFromList [oldItem]
      newDb = eventDBFromList [oldItem, newItem]
      comparison = compareDB oldDb newDb
  comparison @?= [DiffNew newItem]

-- |Delete an element from the DB, see if it's in the "deleted" set
case_compareDbDeleted = do
  let oldItem = dummyAppt "foo"
      newItem = dummyAppt "bar"
      oldDb = eventDBFromList [oldItem, newItem]
      newDb = eventDBFromList [oldItem]
      comparison = compareDB oldDb newDb
  comparison @?= [DiffDeleted newItem]

-- |Modify an element, see if it's flagged as modified
case_compareDbModified = do
  let oldItem = dummyAppt "foo"
      newItem = dummyApptSummary "bar" "firstsummary"
      newItem' = dummyApptSummary "bar" "othersummary"
      oldDb = eventDBFromList [oldItem, newItem]
      newDb = eventDBFromList [oldItem, newItem']
      comparison = compareDB oldDb newDb
  comparison @?= [DiffModified newItem']

-- |See if comparing two DBs that are the same results in...nothing
case_compareDbNotReallyModified = do
  let oldItem = dummyAppt "foo"
      newItem = dummyApptSummary "bar" "firstsummary"
      oldDb = eventDBFromList [oldItem, newItem]
      newDb = eventDBFromList [oldItem, newItem]
      comparison = compareDB oldDb newDb
  comparison @?= []

case_timeOfDayToText =
  let tod = TimeOfDay 16 02 zeroV
  in timeOfDayToText tod @?= "16:02 Uhr"

case_localTimeToText = localTimeToText (readLocalTimeString "2018-10-02T16:01:00") @?= "2.10.2018 16:01 Uhr"

case_dayToText = dayToText (readDayString "2018-10-02") @?= "2.10.2018"

case_formatDateOrDateTimeAllDay =
  formatDateOrDateTime utcTZ (AllDay (readDayString "2018-10-02")) @?= "2.10.2018"

case_formatDateOrDateTimeAtPoint =
  formatDateOrDateTime utcTZ (AtPoint (readTimeString "2018-10-02T16:01:00")) @?= "2.10.2018 16:01 Uhr"

case_formatTimeOnlyStart = formatTime utcTZ (OnlyStart (AllDay (readDayString "2018-10-02"))) @?= "2.10.2018"

case_formatTimeRange = formatTime utcTZ (Range (AllDay (readDayString "2018-10-02")) (AllDay (readDayString "2018-10-03"))) @?= "vom 2.10.2018 bis 3.10.2018"

-- |Single appointment, with just a start date/time, in UTC.
case_collectAppointmentsOnlyStartAtPoint = do
  let start = readTimeString "2017-11-01T10:00:00"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appointment {
          iePath = "/some/path.ical"
        , ieSummary = "foo"
        , ieTime = OnlyStart (AtPoint start)
        , ieUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppointments db utcTZ now)
  result @?= [ (readTimeString "2017-11-01T10:00:00", "Beginnt jetzt: foo")
             , (readTimeString "2017-10-31T20:00:00", "Termin morgen: 10:00 Uhr foo")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: 10:00 Uhr foo")
             ]

-- |Single appointment, with just a start date/time, in Berlin.
case_collectAppointmentsOnlyStartAtPointEurope = do
  tz <- loadTZFromDB "Europe/Berlin"
  let start = readTimeString "2017-11-01T10:00:00"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appointment {
          iePath = "/some/path.ical"
        , ieSummary = "foo"
        , ieTime = OnlyStart (AtPoint start)
        , ieUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppointments db tz now)
  result @?= [ (readTimeString "2017-11-01T10:00:00", "Beginnt jetzt: foo")
             , (readTimeString "2017-10-31T20:00:00", "Termin morgen: 11:00 Uhr foo")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: 11:00 Uhr foo")
             ]

-- |Single appointment, with just a start date, in UTC.
case_collectAppointmentsOnlyStartAllDay = do
  let start = readDayString "2017-11-01"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appointment {
          iePath = "/some/path.ical"
        , ieSummary = "foo"
        , ieTime = OnlyStart (AllDay start)
        , ieUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppointments db utcTZ now)
  result @?= [ (readTimeString "2017-10-31T20:00:00", "Termin morgen: foo (ganztägig)")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: foo (ganztägig)")
             ]

-- |Single appointment, with a start and an end (point), in UTC.
case_collectAppointmentsRange = do
  let start = readTimeString "2017-11-01T10:00:00"
      end = readTimeString "2017-11-02T11:00:00"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appointment {
          iePath = "/some/path.ical"
        , ieSummary = "foo"
        , ieTime = Range (AtPoint start) (AtPoint end)
        , ieUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppointments db utcTZ now)
  result @?= [ (readTimeString "2017-11-01T10:00:00", "Beginnt jetzt: foo")
             , (readTimeString "2017-11-02T11:00:00", "Endet jetzt: foo")
             , (readTimeString "2017-10-31T20:00:00", "Termin morgen: 10:00 Uhr foo")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: 10:00 Uhr foo")
             , (readTimeString "2017-11-01T20:00:00", "Termin morgen: 11:00 Uhr foo endet")
             , (readTimeString "2017-11-02T08:00:00", "Termin heute: 11:00 Uhr foo endet")
             ]

-- |New appointment on the current day
case_collectAppointmentsOnCurrentDay = do
  let start = readTimeString "2017-11-02T10:00:00"
      end = readTimeString "2017-11-03T11:00:00"
      now = readTimeString "2017-11-02T07:00:00"
      firstAppt = Appointment {
          iePath = "/some/path.ical"
        , ieSummary = "foo"
        , ieTime = Range (AtPoint start) (AtPoint end)
        , ieUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppointments db utcTZ now)
  result @?= [ (readTimeString "2017-11-02T10:00:00", "Beginnt jetzt: foo")
             , (readTimeString "2017-11-03T11:00:00", "Endet jetzt: foo")
             , (readTimeString "2017-11-02T08:00:00", "Termin heute: 10:00 Uhr foo")
             , (readTimeString "2017-11-02T20:00:00", "Termin morgen: 11:00 Uhr foo endet")
             , (readTimeString "2017-11-03T08:00:00", "Termin heute: 11:00 Uhr foo endet")
             ]

-- |Appointment is ongoing, an "end" message should still be generated
case_collectAppointmentsEndingOnCurrentDay = do
  let start = readTimeString "2017-11-02T10:00:00"
      end = readTimeString "2017-11-03T11:00:00"
      now = readTimeString "2017-11-02T15:00:00"
      firstAppt = Appointment {
          iePath = "/some/path.ical"
        , ieSummary = "foo"
        , ieTime = Range (AtPoint start) (AtPoint end)
        , ieUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppointments db utcTZ now)
  result @?= [ (readTimeString "2017-11-03T11:00:00", "Endet jetzt: foo")
             , (readTimeString "2017-11-02T20:00:00", "Termin morgen: 11:00 Uhr foo endet")
             , (readTimeString "2017-11-03T08:00:00", "Termin heute: 11:00 Uhr foo endet")
             ]

-- |The end of an appointment should be seen in the day overview
case_collectAppointmentsEndingTomorrow = do
  let start = readTimeString "2017-11-02T10:00:00"
      end = readTimeString "2017-11-05T11:00:00"
      now = readTimeString "2017-11-03T15:00:00"
      firstAppt = Appointment {
          iePath = "/some/path.ical"
        , ieSummary = "foo"
        , ieTime = Range (AtPoint start) (AtPoint end)
        , ieUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppointments db utcTZ now)
  result @?= [ (readTimeString "2017-11-05T11:00:00", "Endet jetzt: foo")
             , (readTimeString "2017-11-04T20:00:00", "Termin morgen: 11:00 Uhr foo endet")
             , (readTimeString "2017-11-05T08:00:00", "Termin heute: 11:00 Uhr foo endet")
             ]

-- |The end of an appointment should be seen in the day overview (all day end)
case_collectAppointmentsEndingTomorrowAllDay = do
  let start = readTimeString "2017-11-02T10:00:00"
      end = readDayString "2017-11-04"
      now = readTimeString "2017-11-03T15:00:00"
      firstAppt = Appointment {
          iePath = "/some/path.ical"
        , ieSummary = "foo"
        , ieTime = Range (AtPoint start) (AllDay end)
        , ieUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppointments db utcTZ now)
  result @?= [ (readTimeString "2017-11-03T20:00:00", "Termin morgen: foo endet")
             , (readTimeString "2017-11-04T08:00:00", "Termin heute: foo endet")
             ]

main :: IO ()
main = $(defaultMainGenerator)
