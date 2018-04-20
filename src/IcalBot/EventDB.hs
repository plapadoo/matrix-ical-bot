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
  , SelectedAppointment(..)
  , AppointmentStatus(..)
  , EventDifference(..)) where


import           Control.Applicative   (pure)
import           Control.Exception     (catch)
import           Control.Monad         (join)
import           Data.Bool             (Bool (False), not, (||))
import           Data.Default          (def)
import           Data.Either           (Either (Left, Right), partitionEithers)
import           Data.Eq               (Eq, (/=))
import           Data.Foldable         (foldr, forM_, toList)
import           Data.Function         (flip, id, ($), (.))
import           Data.Functor          ((<$>))
import           Data.List             (any, concatMap, filter, isInfixOf)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (Maybe, catMaybes, fromJust, maybe)
import           Data.Monoid           (Monoid, mappend, mempty, (<>))
import           Data.Ord              ((>=))
import qualified Data.Set              as Set
import           Data.String           (String)
import qualified Data.Text             as Text
import           Data.Thyme.Calendar   (Day)
import           Data.Thyme.Clock      (UTCTime)
import           Data.Traversable      (for, traverse)
import           Data.Tuple            (fst)
import           IcalBot.Appointment   (Appointment, fromIcal, ieEnd, ieEndDay,
                                        ieStart, ieStartDay, ieTime, ieUid)
import           IcalBot.Util          (Endo, listDirectory, showException)
import           System.FilePath       (FilePath)
import           System.IO             (IO, hPutStrLn, stderr)
import           Text.ICalendar.Parser (parseICalendarFile)
import           Text.ICalendar.Types  (VCalendar (vcEvents), VEvent)
import           Text.Show             (Show)

-- |Wrapper for an event UID (could be a newtype, I'm just lazy)
type EventID = Text.Text

-- |Set of IDs (to make implementation interchangeable)
type EventIDSet = Set.Set EventID

-- |A collection of Appointments (or events, which is shorter), with
-- the UID as primary key
newtype EventDB = EventDB (Map.Map EventID Appointment)
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
data EventDifference = DiffNew Appointment
                     | DiffDeleted Appointment
                     | DiffModified Appointment
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

traverseCalFiles :: [FilePath] -> IO [Either String [Appointment]]
traverseCalFiles files = for files $ \fn -> do
  parsed <- parseICalendarFileSafe fn
  case parsed of
    Left e  -> pure (Left e)
    Right e -> do
      eventResults <- traverse (fromIcal fn) (veventsInCals e)
      pure (Right (catMaybes eventResults))

validEventPath :: FilePath -> Bool
validEventPath e = not (any (`isInfixOf` e) [".Radicale.cache",".Radicale.tmp",".Radicale.props"])

-- |Create an event data base from a plain list
eventDBFromList :: [Appointment] -> EventDB
eventDBFromList = EventDB . foldr (\e -> Map.insert (ieUid e) e) mempty

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

filterDB :: EventDB -> (Appointment -> Bool) -> EventDB
filterDB (EventDB db) f = EventDB (Map.filter f db)

-- |Filter those events which are either starting in the future or are ongoing
filterDBAfter :: EventDB -> UTCTime -> EventDB
filterDBAfter db now = filterDB db f
  where f :: Appointment -> Bool
        f a = ieStart (ieTime a) >= now || maybe False (>= now) (ieEnd (ieTime a))

-- |This type is just to categorize in which context a day "was" selected by daysGrouped
data AppointmentStatus = AppointmentStarts
                       | AppointmentEnds

-- |An appointment that either starts or ends on a specific day
data SelectedAppointment = SelectedAppointment {
    saAppt   :: Appointment
  , saStatus :: AppointmentStatus
  }

-- |Group appointments by day (start and end date)
daysGrouped :: EventDB -> Map.Map Day [SelectedAppointment]
daysGrouped (EventDB db) =
  let accum :: Appointment -> Endo (Map.Map Day [SelectedAppointment])
      accum appt =
        let ins :: AppointmentStatus -> Day -> Endo (Map.Map Day [SelectedAppointment])
            ins s x = Map.insertWith mappend x [SelectedAppointment appt s]
            start :: Endo (Map.Map Day [SelectedAppointment])
            start = ins AppointmentStarts (ieStartDay appt)
            end :: Endo (Map.Map Day [SelectedAppointment])
            end = maybe id (ins AppointmentEnds) (ieEndDay appt)
        in end . start
  in foldr accum mempty (Map.elems db)

collect :: EventDB -> (Appointment -> a) -> [a]
collect (EventDB db) f = f <$> Map.elems db

collectFlat :: EventDB -> (Appointment -> [a]) -> [a]
collectFlat (EventDB db) f = concatMap f (Map.elems db)
