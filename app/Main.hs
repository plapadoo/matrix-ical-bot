module Main where

import           Control.Applicative            ((<*>))
import           Control.Concurrent             (threadDelay)
import           Control.Exception              (SomeException, catch)
import           Control.Monad                  (join, return, void)
import           Data.Bool                      (Bool (..), (&&))
import           Data.Default                   (def)
import           Data.Either                    (Either (..))
import           Data.Eq                        ((/=), (==))
import           Data.Foldable                  (foldMap, null)
import           Data.Function                  (($), (.))
import           Data.Functor                   (Functor, (<$>))
import           Data.Int                       (Int)
import           Data.List                      (concatMap, filter, length)
import           Data.Map.Lazy                  (Map, intersectionWith,
                                                 singleton, toList, (\\))
import           Data.Maybe                     (Maybe (..), fromJust)
import           Data.Monoid                    ((<>))
import           Data.String                    (String)
import           Data.Text.Lazy                 (Text, intercalate, length,
                                                 null, pack, toStrict)
import           Data.Text.Lazy.IO              (putStrLn)
import           Data.Time.Calendar             (Day, toGregorian)
import           Data.Time.Clock                (UTCTime (..))
import           Data.Time.LocalTime            (LocalTime (..), TimeOfDay (..))
import           Data.Tuple                     (fst, snd)
import           Options.Applicative            (Parser, execParser, fullDesc,
                                                 header, help, helper, info,
                                                 long, metavar, progDesc,
                                                 strOption, (<**>))
import           Prelude                        (div, floor, mod, (*))
import           System.FilePath                (FilePath)
import           System.IO                      (IO)
import           Text.ICalendar.Parser          (parseICalendarFile)
import           Text.ICalendar.Types           (DTEnd (..), DTStart (..),
                                                 Date (..), DateTime (..), UID,
                                                 VCalendar, VEvent(..),
                                                 summaryValue, vcEvents)
import           Text.Show                      (Show, show)
import           Web.Matrix.Bot.API             (sendMessage)
import           Web.Matrix.Bot.IncomingMessage (constructIncomingMessage)

data Settings = Settings {
    settingIcalFile     :: FilePath
  , settingMatrixBotUrl :: String
  , settingMatrixRoom   :: String
  }

settingsParser :: Parser Settings
settingsParser =
  Settings
      <$> strOption
          ( long "ical-file"
         <> help "Part to the ical file to parse"
         <> metavar "ICAL")
      <*> strOption
          ( long "matrix-bot-url"
         <> metavar "BOT_URL"
         <> help "URL of the matrix bot to send to" )
      <*> strOption
          ( long "matrix-room"
         <> metavar "ROOM"
         <> help "Internal room ID to send alerts to " )

parseSettings :: IO Settings
parseSettings = execParser opts
  where opts = info (settingsParser <**> helper)
         ( fullDesc
         <> progDesc "Regularly poll ICAL file and send annotations to BOT_URL in room ROOM"
         <> header "matrix-ical-bot - a bot that polls an ical file and pushes changes to matrix" )

data Difference a = Difference {
    diffDeleted   :: [a]
  , diffDiffering :: [(a,a)]
  , diffAdded     :: [a]
  } deriving(Show,Functor)

type EventMap = Map UID VEvent

readVCals :: FilePath -> IO (Either String [VCalendar])
readVCals filePath = (fst <$>) <$> (parseICalendarFile def filePath `catch` (\e -> return (Left $ "error reading file: " <> show (e :: SomeException))))

readEventMap :: FilePath -> IO (Either String EventMap)
readEventMap filePath = (vcalsToMap <$>) <$> readVCals filePath

eventSingleton :: VEvent -> EventMap
eventSingleton e = singleton (veUID e) e

vcalsToMap :: [VCalendar] -> EventMap
vcalsToMap vcals = foldMap eventSingleton (concatMap ((snd <$>) . toList . vcEvents) vcals)

eventMapToEvents :: Map a b -> [b]
eventMapToEvents f = snd <$> toList f

calculateDifference :: EventMap -> EventMap -> [(VEvent,VEvent)]
calculateDifference before after =
  let beforeSingleton = return <$> before
      afterSingleton = return <$> after
      mergedMaps = intersectionWith (<>) beforeSingleton afterSingleton
      mergedValues = snd <$> toList mergedMaps
      twoTuples = filter ((==2) . Data.List.length) mergedValues
   in
    (\[a,b] -> (a,b)) <$> twoTuples

eventDifference :: EventMap -> EventMap -> Difference VEvent
eventDifference before after = Difference {
    diffDeleted = eventMapToEvents (before \\ after)
  , diffDiffering = calculateDifference before after
  , diffAdded =  eventMapToEvents ( after \\ before )
  }

filterEqualElements :: Difference VEvent -> Difference VEvent
filterEqualElements diff = diff {
    diffDiffering = filter (\(a,b) -> a /= b) (diffDiffering diff)
  }

differenceIsEmpty :: Difference a -> Bool
differenceIsEmpty d =
     Data.Foldable.null (diffDiffering d)
  && Data.Foldable.null (diffAdded d)
  && Data.Foldable.null (diffDeleted d)

dayToText :: Day -> Text
dayToText day =
  let (year,month,d) = toGregorian day
  in pack $ " am " <> show d <> "." <> show month <> "." <> show year

textShow :: Int -> Text
textShow = pack . show

timeOfDayToText :: TimeOfDay -> Text
timeOfDayToText (TimeOfDay hour minute _) =
  let minuteText = textShow minute
      hourText = textShow hour
      minutePadded = if Data.Text.Lazy.length minuteText == 1 then "0" <> minuteText else minuteText
  in hourText <> ":" <> minutePadded <> " Uhr"

localTimeToText :: LocalTime -> Text
localTimeToText (LocalTime day timeOfDay) =
  let dayText = dayToText day
      timeOfDayText = timeOfDayToText timeOfDay
  in dayText <> " " <> timeOfDayText

utcTimeToText :: UTCTime -> Text
utcTimeToText (UTCTime day dayTime) =
  let dayText = dayToText day
      dayTimeInt = floor dayTime
      timeOfDay = TimeOfDay (dayTimeInt `div` 60) (dayTimeInt `mod` 60) 0
      dt = timeOfDayToText timeOfDay
  in dayText <> " " <> dt

startDateToText :: DTStart -> Text
startDateToText (DTStartDateTime (FloatingDateTime localTime) _) = localTimeToText localTime
startDateToText (DTStartDateTime (UTCDateTime utcTime) _) = utcTimeToText utcTime
startDateToText (DTStartDateTime (ZonedDateTime localTime timeZone) _) = localTimeToText localTime <> "@" <> timeZone
startDateToText (DTStartDate (Date day) _) = dayToText day

endDateToText :: DTEnd -> Text
endDateToText (DTEndDateTime (FloatingDateTime localTime) _) = localTimeToText localTime
endDateToText (DTEndDateTime (UTCDateTime utcTime) _) = utcTimeToText utcTime
endDateToText (DTEndDateTime (ZonedDateTime localTime timeZone) _) = localTimeToText localTime <> "@" <> timeZone
endDateToText (DTEndDate (Date day) _) = dayToText day

formatEvent :: VEvent -> Text
formatEvent e =
  let summary = (summaryValue . fromJust . veSummary) e
      maybeStart = foldMap startDateToText (veDTStart e)
  in if Data.Text.Lazy.null maybeStart then summary else summary <> maybeStart


differenceToString :: Difference VEvent -> Text
differenceToString diff =
  let summaryDiff = formatEvent <$> diff
      addedValues = intercalate ", " (diffAdded summaryDiff)
      deletedValues = intercalate ", " (diffDeleted summaryDiff)
      changedValues = intercalate "," (fst <$> diffDiffering summaryDiff)
      totalValues = [
          if Data.Text.Lazy.null addedValues then [] else ["added: " <> addedValues]
        , if Data.Text.Lazy.null deletedValues then [] else ["deleted: " <> deletedValues]
        , if Data.Text.Lazy.null changedValues then [] else ["changed: " <> changedValues]]
  in "calendar: " <> intercalate ", " (join totalValues)

delayGlobal :: IO ()
delayGlobal = threadDelay (1000 * 1000)

loopIteration :: Settings -> IO EventMap -> EventMap -> IO ()
loopIteration settings reader previousResult = do
  currentResult <- reader
  let diff = filterEqualElements (previousResult `eventDifference` currentResult)
  if differenceIsEmpty diff
    then do
      delayGlobal
      loopIteration settings reader currentResult
    else do
      delayGlobal
      void $ sendMessage (toStrict (pack (settingMatrixBotUrl settings))) (toStrict (pack (settingMatrixRoom settings))) (constructIncomingMessage (toStrict (differenceToString diff)) Nothing)
      loopIteration settings reader currentResult

readFileWithPauses :: FilePath -> IO EventMap
readFileWithPauses filePath = do
  result' <- readEventMap filePath
  case result' of
    Right result -> return result
    Left errorMessage -> do
      putStrLn (pack errorMessage)
      delayGlobal
      readFileWithPauses filePath

main :: IO ()
main = do
  settings <- parseSettings
  initial <- readFileWithPauses (settingIcalFile settings)
  loopIteration settings (readFileWithPauses (settingIcalFile settings)) initial
