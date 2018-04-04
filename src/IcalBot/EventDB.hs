module IcalBot.EventDB(
    EventDB
  , EventID
  , eventDBFromFS
  , compareDB
  , eventDBFromList
  , eventDBFromFile
  , eventsByUID
  , EventDifference(..)
  , nextNotification) where


import           Control.Applicative   (pure)
import           Control.Exception     (catch)
import           Control.Lens          ((^.))
import           Control.Monad         (join)
import           Data.Bool             (Bool, not)
import           Data.Default          (def)
import           Data.Either           (Either (Left, Right), partitionEithers)
import           Data.Eq               (Eq, (/=))
import           Data.Foldable         (foldr, forM_, toList)
import           Data.Function         (flip, ($), (.))
import           Data.Functor          ((<$>))
import           Data.List             (any, concatMap, filter, isInfixOf)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (Maybe, catMaybes, fromJust)
import           Data.Monoid           (Monoid, mappend, mempty, (<>))
import qualified Data.Set              as Set
import           Data.String           (String)
import qualified Data.Text             as Text
import           Data.Thyme.Clock      (UTCTime)
import           Data.Traversable      (traverse)
import           Data.Tuple            (fst)
import           IcalBot.Appointment   (Appointment, fromIcal, ieUid)
import           IcalBot.Util          (listDirectory, showException)
import           Prelude               (undefined)
import           System.FilePath       (FilePath)
import           System.IO             (IO, hPutStrLn, stderr)
import           Text.ICalendar.Parser (parseICalendarFile)
import           Text.ICalendar.Types  (VCalendar (vcEvents), VEvent)
import           Text.Show             (Show)

type EventID = Text.Text

data EventDB = EventDB (Map.Map EventID Appointment)
             deriving(Eq, Show)

instance Monoid EventDB where
  mempty = EventDB mempty
  (EventDB a) `mappend` (EventDB b) = EventDB (a `mappend` b)

veventsInCals :: [VCalendar] -> [VEvent]
veventsInCals = concatMap (toList . vcEvents)

parseICalendarFileSafe :: FilePath -> IO (Either String [VCalendar])
parseICalendarFileSafe fn = (fst <$>) <$> parseICalendarFile def fn `catch` (pure . Left . showException)

data EventDifference = DiffNew Appointment
                     | DiffDeleted Appointment
                     | DiffModified Appointment
                     deriving(Show, Eq)

flipLook :: Map.Map EventID a -> EventID -> Maybe a
flipLook = flip Map.lookup

compareDB :: EventDB -> EventDB -> [EventDifference]
compareDB (EventDB old) (EventDB new) =
  let deleted = DiffDeleted <$> Map.elems (old `Map.difference` new)
      inserted = DiffNew <$> Map.elems (new `Map.difference` old)
      modifiedKeys :: Set.Set EventID
      modifiedKeys = Map.keysSet old `Set.intersection` Map.keysSet new
      modified = Set.filter (\key -> Map.lookup key old /= Map.lookup key new) modifiedKeys
      afterMod = (DiffModified . fromJust . flipLook new) <$> toList modified
  in inserted <> deleted <> afterMod

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

eventDBFromList :: [Appointment] -> EventDB
eventDBFromList = EventDB . foldr (\e -> Map.insert (e ^. ieUid) e) mempty

eventDBFromFile :: FilePath -> IO EventDB
eventDBFromFile icalFile = do
  eithers <- traverseCalFiles [icalFile]
  let (lefts,rights) = partitionEithers eithers
  -- print all errors, then ignore
  forM_ lefts (hPutStrLn stderr)
  pure (eventDBFromList (join rights))

eventDBFromFS :: FilePath -> IO EventDB
eventDBFromFS icalDir = do
  files <- filter validEventPath <$> listDirectory icalDir
  eithers <- traverseCalFiles files
  let (lefts,rights) = partitionEithers eithers
  -- print all errors, then ignore
  forM_ lefts (hPutStrLn stderr)
  pure (eventDBFromList (join rights))

nextNotification :: EventDB -> [EventID] -> Maybe (UTCTime, [EventID])
nextNotification = undefined

eventsByUID :: EventDB -> [EventID] -> [Appointment]
eventsByUID (EventDB db) uids = catMaybes (flipLook db <$> uids)

