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
import           IcalBot.AppointedTime          (AppointedTime (..))
import           IcalBot.Appt                   (Appt (..))
import           IcalBot.DateOrDateTime         (DateOrDateTime (..))
import           IcalBot.EventDB                (EventDifference (..),
                                                 compareDB, eventDBFromFile,
                                                 eventDBFromList)
import           IcalBot.Formatting             (dayToText,
                                                 formatDateOrDateTime,
                                                 formatTime, localTimeToText,
                                                 timeOfDayToText)
import           IcalBot.MatrixMessage          (messagePlainText, plainMessage)
import           IcalBot.Scheduling             (collectAppts)
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
  let testAppt = Appt {
          apptPath = fn
        , apptSummary = "testevent"
        , apptTime = Range (AtPoint start) (AtPoint end)
        , apptUid = "06c41895-6d2c-44c2-ae29-50fa85692765"
        }
  appt @?= (eventDBFromList [testAppt])

-- |Try to read a pre-created file and see if the data matches
case_eventWithDate = do
  let fn = "test/data/just_date.ical"
  appt <- eventDBFromFile fn
  let testAppt = Appt {
          apptPath = fn
        , apptSummary = "dateevent"
        , apptTime = Range (AllDay (readDayString "2019-05-31")) (AllDay (readDayString "2019-06-03"))
        , apptUid = "f8a596b2-2c5c-4628-831a-1d505da9ae18"
        }
  appt @?= (eventDBFromList [testAppt])

-- |Just an appt with a specific UID
dummyAppt :: Text.Text -> Appt
dummyAppt uid = dummyApptSummary uid uid

-- |Just an appt with a specific UID and a summary
dummyApptSummary :: Text.Text -> Text.Text -> Appt
dummyApptSummary uid summary =
  Appt {
    apptPath = "/some/path.ical"
  , apptSummary = summary
  , apptTime = Range (AllDay (readDayString "2019-05-31")) (AllDay (readDayString "2019-06-03"))
  , apptUid = uid
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

-- |Single appt, with just a start date/time, in UTC.
case_collectApptsOnlyStartAtPoint = do
  let start = readTimeString "2017-11-01T10:00:00"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appt {
          apptPath = "/some/path.ical"
        , apptSummary = "foo"
        , apptTime = OnlyStart (AtPoint start)
        , apptUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppts db utcTZ now)
  result @?= [ (readTimeString "2017-11-01T10:00:00", "Beginnt jetzt: foo")
             , (readTimeString "2017-10-31T20:00:00", "Termin morgen: 10:00 Uhr foo")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: 10:00 Uhr foo")
             ]

-- |Single appt, with just a start date/time, in Berlin.
case_collectApptsOnlyStartAtPointEurope = do
  tz <- loadTZFromDB "Europe/Berlin"
  let start = readTimeString "2017-11-01T10:00:00"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appt {
          apptPath = "/some/path.ical"
        , apptSummary = "foo"
        , apptTime = OnlyStart (AtPoint start)
        , apptUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppts db tz now)
  result @?= [ (readTimeString "2017-11-01T10:00:00", "Beginnt jetzt: foo")
             , (readTimeString "2017-10-31T20:00:00", "Termin morgen: 11:00 Uhr foo")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: 11:00 Uhr foo")
             ]

-- |Single appt, with start date/time, in Berlin, starting and ending on the same day.
case_collectApptsStartAndEndOnSameDay = do
  tz <- loadTZFromDB "Europe/Berlin"
  let start = readTimeString "2017-11-01T10:00:00"
      end = readTimeString "2017-11-01T15:00:00"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appt {
          apptPath = "/some/path.ical"
        , apptSummary = "foo"
        , apptTime = Range (AtPoint start) (AtPoint end)
        , apptUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppts db tz now)
  result @?= [ (readTimeString "2017-11-01T10:00:00", "Beginnt jetzt: foo")
             , (readTimeString "2017-11-01T15:00:00", "Endet jetzt: foo")
             , (readTimeString "2017-10-31T20:00:00", "Termin morgen: 11:00 Uhr foo")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: 11:00 Uhr foo")
             ]

-- |Single appt, with just a start date, in UTC.
case_collectApptsOnlyStartAllDay = do
  let start = readDayString "2017-11-01"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appt {
          apptPath = "/some/path.ical"
        , apptSummary = "foo"
        , apptTime = OnlyStart (AllDay start)
        , apptUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppts db utcTZ now)
  result @?= [ (readTimeString "2017-10-31T20:00:00", "Termin morgen: foo (ganztägig)")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: foo (ganztägig)")
             ]

-- |Single appt, with a start and an end (point), in UTC.
case_collectApptsRange = do
  let start = readTimeString "2017-11-01T10:00:00"
      end = readTimeString "2017-11-02T11:00:00"
      now = readTimeString "2017-10-01T10:00:00"
      firstAppt = Appt {
          apptPath = "/some/path.ical"
        , apptSummary = "foo"
        , apptTime = Range (AtPoint start) (AtPoint end)
        , apptUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppts db utcTZ now)
  result @?= [ (readTimeString "2017-11-01T10:00:00", "Beginnt jetzt: foo")
             , (readTimeString "2017-11-02T11:00:00", "Endet jetzt: foo")
             , (readTimeString "2017-10-31T20:00:00", "Termin morgen: 10:00 Uhr foo")
             , (readTimeString "2017-11-01T08:00:00", "Termin heute: 10:00 Uhr foo")
             , (readTimeString "2017-11-01T20:00:00", "Termin morgen: 11:00 Uhr foo endet")
             , (readTimeString "2017-11-02T08:00:00", "Termin heute: 11:00 Uhr foo endet")
             ]

-- |New appt on the current day
case_collectApptsOnCurrentDay = do
  let start = readTimeString "2017-11-02T10:00:00"
      end = readTimeString "2017-11-03T11:00:00"
      now = readTimeString "2017-11-02T07:00:00"
      firstAppt = Appt {
          apptPath = "/some/path.ical"
        , apptSummary = "foo"
        , apptTime = Range (AtPoint start) (AtPoint end)
        , apptUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppts db utcTZ now)
  result @?= [ (readTimeString "2017-11-02T10:00:00", "Beginnt jetzt: foo")
             , (readTimeString "2017-11-03T11:00:00", "Endet jetzt: foo")
             , (readTimeString "2017-11-02T08:00:00", "Termin heute: 10:00 Uhr foo")
             , (readTimeString "2017-11-02T20:00:00", "Termin morgen: 11:00 Uhr foo endet")
             , (readTimeString "2017-11-03T08:00:00", "Termin heute: 11:00 Uhr foo endet")
             ]

-- |Appt is ongoing, an "end" message should still be generated
case_collectApptsEndingOnCurrentDay = do
  let start = readTimeString "2017-11-02T10:00:00"
      end = readTimeString "2017-11-03T11:00:00"
      now = readTimeString "2017-11-02T15:00:00"
      firstAppt = Appt {
          apptPath = "/some/path.ical"
        , apptSummary = "foo"
        , apptTime = Range (AtPoint start) (AtPoint end)
        , apptUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppts db utcTZ now)
  result @?= [ (readTimeString "2017-11-03T11:00:00", "Endet jetzt: foo")
             , (readTimeString "2017-11-02T20:00:00", "Termin morgen: 11:00 Uhr foo endet")
             , (readTimeString "2017-11-03T08:00:00", "Termin heute: 11:00 Uhr foo endet")
             ]

-- |The end of an appt should be seen in the day overview
case_collectApptsEndingTomorrow = do
  let start = readTimeString "2017-11-02T10:00:00"
      end = readTimeString "2017-11-05T11:00:00"
      now = readTimeString "2017-11-03T15:00:00"
      firstAppt = Appt {
          apptPath = "/some/path.ical"
        , apptSummary = "foo"
        , apptTime = Range (AtPoint start) (AtPoint end)
        , apptUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppts db utcTZ now)
  result @?= [ (readTimeString "2017-11-05T11:00:00", "Endet jetzt: foo")
             , (readTimeString "2017-11-04T20:00:00", "Termin morgen: 11:00 Uhr foo endet")
             , (readTimeString "2017-11-05T08:00:00", "Termin heute: 11:00 Uhr foo endet")
             ]

-- |The end of an appt should be seen in the day overview (all day end)
case_collectApptsEndingTomorrowAllDay = do
  let start = readTimeString "2017-11-02T10:00:00"
      end = readDayString "2017-11-04"
      now = readTimeString "2017-11-03T15:00:00"
      firstAppt = Appt {
          apptPath = "/some/path.ical"
        , apptSummary = "foo"
        , apptTime = Range (AtPoint start) (AllDay end)
        , apptUid = "uid"
        }
      db = eventDBFromList [firstAppt]
      result = second messagePlainText <$> (collectAppts db utcTZ now)
  result @?= [ (readTimeString "2017-11-03T20:00:00", "Termin morgen: foo endet")
             , (readTimeString "2017-11-04T08:00:00", "Termin heute: foo endet")
             ]

main :: IO ()
main = $(defaultMainGenerator)
