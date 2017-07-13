module Main where

import           Config                         (Config, configBotUrl,
                                                 configIcalDir, configRoom)
import           Control.Applicative            (pure)
import           Data.Eq                        ((==))
import           Data.Int                       (Int)

import           Control.Concurrent             (newChan, readChan)
import           Control.Exception              (SomeException, catch)
import           Control.Monad                  (Monad, forever, void, when)
import           Data.Bool                      (Bool (..))
import           Data.Default                   (def)
import           Data.Either                    (Either (..))
import           Data.Foldable                  (concatMap, foldMap)
import           Data.Function                  (const, flip, ($), (.))
import           Data.Functor                   ((<$>))
import           Data.Map.Lazy                  (toList)
import           Data.Maybe                     (Maybe (..), fromJust)
import           Data.Monoid                    (Monoid, (<>))
import           Data.Ord                       ((<))
import           Data.String                    (IsString (fromString), String)
import qualified Data.Text                      as StrictText
import qualified Data.Text.Lazy                 as LazyText
import           Data.Time.Calendar             (Day, toGregorian)
import           Data.Time.Clock                (UTCTime (..))
import           Data.Time.LocalTime            (LocalTime (..), TimeOfDay (..))
import           Data.Tuple                     (snd)
import           Dhall                          (auto, input)
import           Lucid                          (Html)
import           Prelude                        (div, floor, mod, (*))
import           System.Directory               (doesFileExist)
import           System.FSNotify                (Event (..), watchTreeChan,
                                                 withManager)
import           System.IO                      (FilePath, IO)
import           Text.ICalendar.Parser          (parseICalendarFile)
import           Text.ICalendar.Types           (DTEnd (..), DTStart (..),
                                                 Date (..), DateTime (..), UID,
                                                 VEvent, summaryValue, vcEvents,
                                                 veDTStart, veSummary)
import           Text.Show                      (show)
import           Web.Matrix.Bot.API             (sendMessage)
import           Web.Matrix.Bot.IncomingMessage (IncomingMessage,
                                                 constructIncomingMessage)

whenM :: Monad m => m Bool -> m () -> m ()
whenM b a = do
  b' <- b
  when b' a

processIcalFile :: FilePath -> (IncomingMessage StrictText.Text (Html ()) -> IO ()) -> StrictText.Text -> IO ()
processIcalFile fn mySendMessage mode = do
  let showException :: SomeException -> String
      showException e = show e
  result <- parseICalendarFile def fn `catch` (pure . Left . showException)
  case result of
    Left e ->
      let errorMessage = "error reading ical file “" <> fromString fn <> "”: " <> fromString e
      in mySendMessage (constructIncomingMessage errorMessage Nothing)
    Right (vcals,_) ->
      let vevents :: [VEvent]
          vevents = concatMap ((snd <$>) . toList . vcEvents) vcals
          appendMode :: StrictText.Text -> StrictText.Text
          appendMode x = mode <> ": " <> x
          simpleMessage :: StrictText.Text -> IncomingMessage StrictText.Text (Html ())
          simpleMessage = flip constructIncomingMessage Nothing
          forEvent :: VEvent -> IO ()
          forEvent = mySendMessage . simpleMessage . appendMode . LazyText.toStrict . formatEvent
      in foldMap forEvent vevents

processEvent :: Config -> Event -> IO ()
processEvent config event = do
  let mySendMessage m = void $ sendMessage (configBotUrl config) (configRoom config) m
  case event of
    Added fn _    -> processIcalFile fn mySendMessage "added"
    Modified fn _ -> processIcalFile fn mySendMessage "modified"
    Removed fn _  ->
      let message = constructIncomingMessage ("ical file “"<> (fromString fn) <>"” was removed") Nothing
      in mySendMessage message

main :: IO ()
main = do
  config <- input auto "/etc/matrix-bot/matrix-ical-bot.dhall"
  withManager $ \mgr -> do
    eventChan <- newChan
    -- start a watching job (in the background)
    stopListening <- watchTreeChan
      mgr
      (configIcalDir config)
      (const True)
      eventChan

    forever $ do
      event <- readChan eventChan
      processEvent config event

dayToText :: IsString a => Day -> a
dayToText day =
  let (year,month,d) = toGregorian day
  in fromString $ " am " <> show d <> "." <> show month <> "." <> show year

textShow :: IsString a => Int -> a
textShow = fromString . show

timeOfDayToText :: (IsString a,Monoid a) => TimeOfDay -> a
timeOfDayToText (TimeOfDay hour minute _) =
  let minuteText = textShow minute
      hourText = textShow hour
      minutePadded = if minute < 10 then "0" <> minuteText else minuteText
  in hourText <> ":" <> minutePadded <> " Uhr"

localTimeToText :: (IsString a,Monoid a) => LocalTime -> a
localTimeToText (LocalTime day timeOfDay) =
  let dayText = dayToText day
      timeOfDayText = timeOfDayToText timeOfDay
  in dayText <> " " <> timeOfDayText

utcTimeToText :: (IsString a,Monoid a) => UTCTime -> a
utcTimeToText (UTCTime day dayTime) =
  let dayText = dayToText day
      dayTimeInt = floor dayTime
      timeOfDay = TimeOfDay (dayTimeInt `div` 60) (dayTimeInt `mod` 60) 0
      dt = timeOfDayToText timeOfDay
  in dayText <> " " <> dt

startDateToText :: DTStart -> LazyText.Text
startDateToText (DTStartDateTime (FloatingDateTime localTime) _) = localTimeToText localTime
startDateToText (DTStartDateTime (UTCDateTime utcTime) _) = utcTimeToText utcTime
startDateToText (DTStartDateTime (ZonedDateTime localTime timeZone) _) = localTimeToText localTime <> "@" <> timeZone
startDateToText (DTStartDate (Date day) _) = dayToText day

endDateToText :: DTEnd -> LazyText.Text
endDateToText (DTEndDateTime (FloatingDateTime localTime) _) = localTimeToText localTime
endDateToText (DTEndDateTime (UTCDateTime utcTime) _) = utcTimeToText utcTime
endDateToText (DTEndDateTime (ZonedDateTime localTime timeZone) _) = localTimeToText localTime <> "@" <> timeZone
endDateToText (DTEndDate (Date day) _) = dayToText day

formatEvent :: VEvent -> LazyText.Text
formatEvent e =
  case veSummary e of
    Nothing -> "event has no summary, cannot generate textual representation"
    Just summary' ->
      let summary = summaryValue summary'
      in case veDTStart e of
           Nothing        -> summary
           Just startDate -> summary <> (startDateToText startDate)
