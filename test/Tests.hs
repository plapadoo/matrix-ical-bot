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
