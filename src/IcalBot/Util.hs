-- |Random utility functions
module IcalBot.Util where

import           Control.Exception (SomeException)
import           Data.Foldable     (minimumBy)
import           Data.Functor      ((<$>))
import           Data.Maybe        (Maybe (Just, Nothing))
import           Data.Ord          (Ordering)
import           Data.String       (String)
import           Prelude           ()
import qualified System.Directory  as Directory
import           System.FilePath   (FilePath, (</>))
import           System.IO         (IO)
import           Text.Show         (show)

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
