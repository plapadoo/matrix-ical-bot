module Main where

import           Control.Applicative            ((<*>))
import           Control.Concurrent             (threadDelay)
import           Control.Exception              (SomeException, catch)
import           Control.Monad                  (return)
import           Data.Bool                      (Bool (..), (&&))
import           Data.Default                   (def)
import           Data.Either                    (Either (..))
import           Data.Eq                        (Eq, (/=), (==))
import           Data.Foldable                  (foldMap, null)
import           Data.Function                  (($), (.))
import           Data.Functor                   (Functor, (<$>))
import           Data.List                      (concatMap, filter, length)
import           Data.Map.Lazy                  (Map, filterWithKey,
                                                 intersection, intersectionWith,
                                                 mapKeys, singleton, toList,
                                                 (\\))
import           Data.Maybe                     (fromJust)
import           Data.Monoid                    ((<>))
import           Data.String                    (String)
import           Data.Text.Lazy                 (Text, intercalate, pack)
import           Data.Text.Lazy.IO              (putStrLn)
import           Data.Tuple                     (fst, snd)
import           Options.Applicative            (Parser, auto, execParser,
                                                 fullDesc, header, help, helper,
                                                 info, long, metavar, option,
                                                 progDesc, strOption, (<**>))
import           Prelude                        (undefined, (*))
import           System.FilePath                (FilePath)
import           System.IO                      (IO)
import           Text.ICalendar.Parser          (parseICalendarFile)
import           Text.ICalendar.Types           (UID, VCalendar, VEvent,
                                                 summaryValue, vcEvents,
                                                 veSummary, veUID)
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
      twoTuples = filter ((==2) . length) mergedValues
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
differenceIsEmpty d = null (diffDiffering d) && null (diffAdded d) && null (diffDeleted d)

differenceToString :: Difference VEvent -> Text
differenceToString diff =
  let summaryDiff = (summaryValue . fromJust . veSummary) <$> diff
  in "added: " <> intercalate ", " (diffAdded summaryDiff) <> ", removed: " <> intercalate ", " (diffDeleted summaryDiff) <> ", changed: " <> intercalate "," (fst <$> diffDiffering summaryDiff)

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
      result <- sendMessage (pack (settingMatrixBotUrl settings)) (pack (settingMatrixRoom settings)) (constructIncomingMessage (differenceToString diff) Nothing)
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
  loopIteration (readFileWithPauses (settingIcalFile settings)) initial
