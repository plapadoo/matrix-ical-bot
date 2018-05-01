-- |Provide the EventDB type which provides a context for comparing events
module IcalBot.EventDB(
    EventDB
  , EventID
  , EventIDSet
  , filterDB
  , filterDBAfter
  , daysGrouped
  , collect
  , collectFlat
  , eventDBFromFS
  , compareDB
  , eventDBFromList
  , eventDBFromFile
  , EventDifference(..)) where


import           Control.Applicative    (pure)
import           Control.Exception      (catch)
import           Control.Monad          (join)
import           Data.Bool              (Bool (False), not, (||))
import           Data.Default           (def)
import           Data.Either            (Either (Left, Right), partitionEithers)
import           Data.Eq                (Eq, (/=))
import           Data.Foldable          (foldMap, foldr, forM_, toList)
import           Data.Function          (flip, ($), (.))
import           Data.Functor           ((<$>))
import           Data.List              (any, concatMap, filter, isInfixOf)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (Maybe, catMaybes, fromJust, maybe)
import           Data.Monoid            (Monoid, mappend, mempty, (<>))
import           Data.Ord               ((>=))
import qualified Data.Set               as Set
import           Data.String            (String)
import qualified Data.Text              as Text
import           Data.Thyme.Calendar    (Day)
import           Data.Thyme.Clock       (UTCTime)
import           Data.Traversable       (for, traverse)
import           Data.Tuple             (fst)
import           IcalBot.AppointedTime  (appTimeEndUtc, appTimeStartUtc)
import           IcalBot.Appt           (Appt, apptTime, apptUid, fromIcal)
import           IcalBot.DateOrDateTime (dayForDateTime)
import           IcalBot.SubAppt        (SubAppt (saTime), appointmentDates)
import           IcalBot.Util           (Endo, listDirectory, showException)
import           System.FilePath        (FilePath)
import           System.IO              (IO, hPutStrLn, stderr)
import           Text.ICalendar.Parser  (parseICalendarFile)
import           Text.ICalendar.Types   (VCalendar (vcEvents), VEvent)
import           Text.Show              (Show)

-- |Wrapper for an event UID (could be a newtype, I'm just lazy)
type EventID = Text.Text

-- |Set of IDs (to make implementation interchangeable)
type EventIDSet = Set.Set EventID

-- |A collection of Appts (or events, which is shorter), with
-- the UID as primary key
newtype EventDB = EventDB (Map.Map EventID Appt)
                deriving(Eq, Show)

instance Monoid EventDB where
  mempty = EventDB mempty
  (EventDB a) `mappend` (EventDB b) = EventDB (a `mappend` b)

veventsInCals :: [VCalendar] -> [VEvent]
veventsInCals = concatMap (toList . vcEvents)

parseICalendarFileSafe :: FilePath -> IO (Either String [VCalendar])
parseICalendarFileSafe fn = (fst <$>) <$> parseICalendarFile def fn `catch` (pure . Left . showException)

-- |Contrary to the name, this isn't a difference of an event, but of
-- two event collections, which resulted in a single event difference.
data EventDifference = DiffNew Appt
                     | DiffDeleted Appt
                     | DiffModified Appt
                     deriving(Show, Eq)

flipLook :: Map.Map EventID a -> EventID -> Maybe a
flipLook = flip Map.lookup

-- |Compare two event databases, return differences
compareDB :: EventDB -> EventDB -> [EventDifference]
compareDB (EventDB old) (EventDB new) =
  let deleted = DiffDeleted <$> Map.elems (old `Map.difference` new)
      inserted = DiffNew <$> Map.elems (new `Map.difference` old)
      modifiedKeys :: EventIDSet
      modifiedKeys = Map.keysSet old `Set.intersection` Map.keysSet new
      modified = Set.filter (\key -> Map.lookup key old /= Map.lookup key new) modifiedKeys
      afterMod = DiffModified . fromJust . flipLook new <$> toList modified
  in inserted <> deleted <> afterMod

traverseCalFiles :: [FilePath] -> IO [Either String [Appt]]
traverseCalFiles files = for files $ \fn -> do
  parsed <- parseICalendarFileSafe fn
  case parsed of
    Left e  -> pure (Left e)
    Right e -> Right . catMaybes <$> traverse (fromIcal fn) (veventsInCals e)

validEventPath :: FilePath -> Bool
validEventPath e = not (any (`isInfixOf` e) [".Radicale.cache",".Radicale.tmp",".Radicale.props"])

-- |Create an event data base from a plain list
eventDBFromList :: [Appt] -> EventDB
eventDBFromList = EventDB . foldr (\e -> Map.insert (apptUid e) e) mempty

eventDBFromFiles :: [FilePath] -> IO EventDB
eventDBFromFiles files = do
  eithers <- traverseCalFiles files
  let (lefts,rights) = partitionEithers eithers
  -- print all errors, then ignore
  forM_ lefts (hPutStrLn stderr)
  pure (eventDBFromList (join rights))

-- |Create an event data base from a single file
eventDBFromFile :: FilePath -> IO EventDB
eventDBFromFile icalFile = eventDBFromFiles [icalFile]

-- |Create an event data base from a directory of ical files
eventDBFromFS :: FilePath -> IO EventDB
eventDBFromFS icalDir = do
  files <- filter validEventPath <$> listDirectory icalDir
  eventDBFromFiles files

filterDB :: EventDB -> (Appt -> Bool) -> EventDB
filterDB (EventDB db) f = EventDB (Map.filter f db)

-- |Filter those events which are either starting in the future or are ongoing
filterDBAfter :: EventDB -> UTCTime -> EventDB
filterDBAfter db now = filterDB db f
  where f :: Appt -> Bool
        f a = appTimeStartUtc (apptTime a) >= now || maybe False (>= now) (appTimeEndUtc (apptTime a))

-- |Group appointments by day (start and end date)
daysGrouped :: EventDB -> Map.Map Day [SubAppt]
daysGrouped (EventDB db) =
  let -- |Put one selected appointment into the corresponding map (create an Endo)
      accumSA :: SubAppt -> Endo (Map.Map Day [SubAppt])
      accumSA appt = Map.insertWith mappend (dayForDateTime (saTime appt)) [appt]
      -- |Fold all the Endos together
      accum :: Appt -> Endo (Map.Map Day [SubAppt])
      accum a = foldMap accumSA (appointmentDates a)
  in foldr accum mempty (Map.elems db)

collect :: EventDB -> (Appt -> a) -> [a]
collect (EventDB db) f = f <$> Map.elems db

collectFlat :: EventDB -> (Appt -> [a]) -> [a]
collectFlat (EventDB db) f = concatMap f (Map.elems db)
