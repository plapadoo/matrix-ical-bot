-- |Random utility functions
module IcalBot.Util where

import           Control.Applicative  (Applicative, pure)
import           Control.Exception    (SomeException)
import           Control.Lens         (from, view, (^.))
import           Data.AdditiveGroup   (zeroV)
import           Data.Eq              ((==))
import           Data.Foldable        (minimumBy)
import           Data.Function        ((.))
import           Data.Functor         ((<$>))
import           Data.List            (filter, minimum)
import           Data.Maybe           (Maybe (Just, Nothing))
import           Data.Ord             (Ord, Ordering)
import           Data.String          (String)
import           Data.Thyme.Calendar  (Day)
import           Data.Thyme.Clock     (UTCTime)
import           Data.Thyme.LocalTime (Hour, LocalTime (..), TimeOfDay (..),
                                       utc, utcLocalTime, _localTimeOfDay)
import           Data.Thyme.Time.Core (fromThyme, toThyme)
import           Data.Time.Zones      (TZ, utcToLocalTimeTZ)
import           Prelude              ()
import qualified System.Directory     as Directory
import           System.FilePath      (FilePath, (</>))
import           System.IO            (IO)
import           Text.Show            (show)

-- |Handy helper type for denoting endomorphisms
type Endo a = a -> a

foldMaybe :: Applicative m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
foldMaybe Nothing _  = pure Nothing
foldMaybe (Just x) f = f x

foldMaybe' :: Applicative m => Maybe a -> (a -> m b) -> m (Maybe b)
foldMaybe' Nothing _  = pure Nothing
foldMaybe' (Just x) f = Just <$> f x

utcTimeAtTz :: TZ -> UTCTime -> LocalTime
utcTimeAtTz tz t =
  let tnative = fromThyme t
      tzi = utcToLocalTimeTZ tz tnative
  in toThyme tzi

timeOfDayAtTz :: TZ -> UTCTime -> TimeOfDay
timeOfDayAtTz tz t =
  let tnative = fromThyme t
      tzi = utcToLocalTimeTZ tz tnative
  in toThyme tzi ^. _localTimeOfDay

-- |List a directory, appending the original path to the resulting paths
listDirectory :: FilePath -> IO [FilePath]
listDirectory d = ((d </>) <$>) <$> Directory.listDirectory d

-- |Minimum of a list, respecting empty lists
minimumBySafe :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBySafe _ [] = Nothing
minimumBySafe f xs = Just (minimumBy f xs)

-- |Turn an exception to a string
showException :: SomeException -> String
showException = show

-- |Create a UTC time from a local time (gluing the zero time zone on)
localTimeToUTCTime :: LocalTime -> UTCTime
localTimeToUTCTime = view (from (utcLocalTime utc))

-- |Create a UTC time from its constituent parts
makeUtcTime :: Day -> TimeOfDay -> UTCTime
makeUtcTime day tod = localTimeToUTCTime (LocalTime day tod)

headSafe :: [a] -> Maybe a
headSafe []    = Nothing
headSafe (x:_) = Just x

-- |Create a time of day at a specific hour
hour :: Hour -> TimeOfDay
hour h = TimeOfDay h 0 zeroV

minimumElementsBy :: Ord a => (b -> a) -> [b] -> [b]
minimumElementsBy _ [] = []
minimumElementsBy f xs =
  let m = minimum (f <$> xs)
  in filter ((== m) . f) xs

minimumSafe :: Ord a => [a] -> Maybe a
minimumSafe [] = Nothing
minimumSafe xs = Just (minimum xs)
