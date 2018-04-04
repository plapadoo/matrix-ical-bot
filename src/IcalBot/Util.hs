module IcalBot.Util where

import           Control.Exception (SomeException)
import           Control.Monad     (Monad, when)
import           Data.Bool         (Bool)
import           Data.Either       (Either (Left, Right), either,
                                    partitionEithers)
import           Data.Foldable     (minimumBy)
import           Data.Functor      ((<$>))
import           Data.Maybe        (Maybe (Just, Nothing), maybe)
import           Data.Ord          (Ordering)
import           Data.String       (String)
import           Prelude           ()
import qualified System.Directory  as Directory
import           System.FilePath   (FilePath, (</>))
import           System.IO         (IO)
import           Text.Show         (show)

listDirectory :: FilePath -> IO [FilePath]
listDirectory d = ((d </>) <$>) <$> Directory.listDirectory d

firstLeftOrRights :: [Either a b] -> Either a [b]
firstLeftOrRights eithers =
  case partitionEithers eithers of
    (left:_,_) -> Left left
    (_,rights) -> Right rights

eitherRight :: Either a b -> (a -> c) -> (b -> c) -> c
eitherRight e f g = either f g e

maybeJust :: Maybe a -> b -> (a -> b) -> b
maybeJust m c f = maybe c f m

minimumBySafe :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBySafe _ [] = Nothing
minimumBySafe f xs = Just (minimumBy f xs)

whenM :: Monad m => m Bool -> m () -> m ()
whenM b a = do
  b' <- b
  when b' a

showException :: SomeException -> String
showException = show
