module IcalBot.EventDB(
    EventDB
  , EventID
  , eventDBFromFS
  , formatEvents
  , compareDB
  , formatDiffs
  , formatEventsUID
  , nextNotification) where


import           Control.Applicative           (pure)
import           Control.Exception             (catch)
import           Control.Lens                  ((^.))
import           Control.Monad                 (join)
import           Data.Bool                     (Bool, not)
import           Data.Default                  (def)
import           Data.Either                   (Either (Left, Right),
                                                partitionEithers)
import           Data.Eq                       ((==))
import           Data.Foldable                 (foldr, forM_, toList)
import           Data.Function                 (flip, ($), (.))
import           Data.Functor                  ((<$>))
import           Data.Int                      (Int)
import           Data.List                     (any, concatMap, filter,
                                                isInfixOf)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (Maybe (Just, Nothing),
                                                catMaybes, fromJust)
import           Data.Monoid                   (mempty, (<>))
import           Data.Ord                      ((<))
import qualified Data.Set                      as Set
import           Data.String                   (IsString (fromString), String)
import qualified Data.Text                     as Text
import           Data.Thyme.Calendar           (Day, gregorian, _ymdDay,
                                                _ymdMonth, _ymdYear)
import           Data.Thyme.Clock              (UTCTime, _utctDay, _utctDayTime)
import           Data.Thyme.LocalTime          (TimeOfDay (..), timeOfDay)
import           Data.Traversable              (traverse)
import           Data.Tuple                    (fst)
import           IcalBot.Appointment           (Appointment,
                                                DateOrDateTime (AllDay, AtPoint),
                                                InternalTime (OnlyStart, Range),
                                                fromIcal, ieSummary, ieTime,
                                                ieUid)
import           IcalBot.MatrixIncomingMessage (IncomingMessage, plainMessage)
import           IcalBot.Util                  (listDirectory, showException)
import           Prelude                       (undefined)
import           System.FilePath               (FilePath)
import           System.IO                     (IO, hPutStrLn, stderr)
import           Text.ICalendar.Parser         (parseICalendarFile)
import           Text.ICalendar.Types          (VCalendar (vcEvents), VEvent)
import           Text.Show                     (Show, show)

type EventID = Text.Text

data EventDB = EventDB (Map.Map EventID Appointment)

veventsInCals :: [VCalendar] -> [VEvent]
veventsInCals = concatMap (toList . vcEvents)

parseICalendarFileSafe :: FilePath -> IO (Either String [VCalendar])
parseICalendarFileSafe fn = (fst <$>) <$> parseICalendarFile def fn `catch` (pure . Left . showException)

data EventDifference = DiffNew Appointment
                     | DiffDeleted Appointment
                     | DiffModified Appointment
                     deriving(Show)

flipLook :: Map.Map EventID a -> EventID -> Maybe a
flipLook = flip Map.lookup

compareDB :: EventDB -> EventDB -> [EventDifference]
compareDB (EventDB old) (EventDB new) =
  let deleted = DiffDeleted <$> Map.elems (old `Map.difference` new)
      inserted = DiffNew <$> Map.elems (new `Map.difference` old)
      modifiedKeys :: Set.Set EventID
      modifiedKeys = Map.keysSet old `Set.intersection` Map.keysSet new
      modified = Set.filter (\key -> Map.lookup key old == Map.lookup key new) modifiedKeys
      afterMod = (DiffModified . fromJust . flipLook new) <$> toList modified
  in inserted <> deleted <> afterMod

dayToText :: Day -> Text.Text
dayToText day =
  let ymd = day ^. gregorian
  in Text.pack $ show (ymd ^. _ymdDay) <> "." <> show (ymd ^. _ymdMonth) <> "." <> show (ymd ^. _ymdYear)

utcTimeToText :: UTCTime -> Text.Text
utcTimeToText t =
  let dayText :: Text.Text
      dayText = dayToText (t ^. _utctDay)
      dayTimeInt = t ^. _utctDayTime . timeOfDay
      dt :: Text.Text
      dt = timeOfDayToText dayTimeInt
  in dayText <> " " <> dt

formatDateOrDateTime :: DateOrDateTime -> Text.Text
formatDateOrDateTime (AllDay day)      = dayToText day
formatDateOrDateTime (AtPoint utcTime) = utcTimeToText utcTime

textShow :: IsString a => Int -> a
textShow = fromString . show

timeOfDayToText :: TimeOfDay -> Text.Text
timeOfDayToText (TimeOfDay hour minute _) =
  let minuteText = textShow minute
      hourText = textShow hour
      minutePadded = if minute < 10 then "0" <> minuteText else minuteText
  in hourText <> ":" <> minutePadded <> " Uhr"

formatTime :: InternalTime -> Text.Text
formatTime (OnlyStart dateOrDt) = formatDateOrDateTime dateOrDt
formatTime (Range start end)    = "Von " <> formatDateOrDateTime start <> " bis " <> formatDateOrDateTime end

formatEventAsText :: Appointment -> Text.Text
formatEventAsText e = (e ^. ieSummary) <> " " <> formatTime (e ^. ieTime)

formatDiffAsText :: EventDifference -> Text.Text
formatDiffAsText (DiffNew e)      = "Neuer Termin: " <> formatEventAsText e
formatDiffAsText (DiffDeleted e)  = "Termin entfernt: " <> formatEventAsText e
formatDiffAsText (DiffModified e) = "Termin anders: " <> formatEventAsText e

formatDiffs :: [EventDifference] -> Maybe IncomingMessage
formatDiffs [] = Nothing
formatDiffs xs = Just (plainMessage (Text.intercalate "," (formatDiffAsText <$> xs)))

formatEvents :: [Appointment] -> Maybe IncomingMessage
formatEvents [] = Nothing
formatEvents xs = Just (plainMessage (Text.intercalate "," (formatEventAsText <$> xs)))

traverseCalFiles :: [FilePath] -> IO [Either String [Appointment]]
traverseCalFiles files = (flip traverse) files $ \fn -> do
  parsed <- parseICalendarFileSafe fn
  case parsed of
    Left e  -> pure (Left e)
    Right e -> do
      eventResults <- traverse (fromIcal fn) (veventsInCals e)
      pure (Right (catMaybes eventResults))

validEventPath :: FilePath -> Bool
validEventPath e = not (any (`isInfixOf` e) [".Radicale.cache",".Radicale.tmp",".Radicale.props"])

eventDBFromFS :: FilePath -> IO EventDB
eventDBFromFS icalDir = do
  files <- filter validEventPath <$> listDirectory icalDir
  eithers <- traverseCalFiles files
  let (lefts,rights) = partitionEithers eithers
  -- print all errors, then ignore
  forM_ lefts (hPutStrLn stderr)
  let events :: [Appointment]
      events = join rights
      insertion e = Map.insert (e ^. ieUid) e
  pure (EventDB (foldr insertion mempty events))

nextNotification :: EventDB -> [EventID] -> Maybe (UTCTime, [EventID])
nextNotification = undefined

formatEventsUID :: EventDB -> [EventID] -> Maybe IncomingMessage
formatEventsUID (EventDB m) uids = formatEvents (catMaybes (flipLook m <$> uids))
