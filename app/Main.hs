module Main where

import           Control.Applicative     (pure)
import           Control.Concurrent      (ThreadId, forkIO, killThread,
                                          threadDelay)
import           Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar,
                                          putMVar)
import           Control.Lens            ((^.))
import           Control.Monad           (void)
import           Data.AffineSpace        ((.-.))
import           Data.Bool               (Bool (..))
import           Data.Foldable           (for_)
import           Data.Function           (const, ($), (.))
import           Data.Maybe              (Maybe (..))
import           Data.Monoid             (mempty)
import qualified Data.Text.IO            as TextIO
import           Data.Thyme.Clock        (getCurrentTime, microseconds)
import           IcalBot.EventDB         (EventDB, EventID, compareDB,
                                          eventDBFromFS, eventsByUID,
                                          nextNotification)
import           IcalBot.Formatting      (formatDiffs, formatEvents)
import           IcalBot.MatrixMessage   (MatrixMessage (..),
                                          incomingMessageToText)
import           Prelude                 (fromIntegral)
import           ProgramOptions          (poDirectory, readProgramOptions)
import           System.FilePath
import           System.FSNotify         (Event (..), watchTree, withManager)
import           System.IO               (BufferMode (NoBuffering), IO,
                                          hSetBuffering, stdout)

newtype WaitJob = WaitJob { getWaitTid :: ThreadId }

data ProgramState = ProgramState EventDB FilePath (Maybe WaitJob)

sendMessage :: MatrixMessage -> IO ()
sendMessage = TextIO.putStrLn . incomingMessageToText

newWaitJob :: EventDB -> MVar ProgramState -> [EventID] -> IO (Maybe WaitJob)
newWaitJob db stateVar excludeUids = do
  case nextNotification db excludeUids of
    Nothing -> pure Nothing
    Just (pointInFuture, uids) -> do
      backThread <- forkIO $ do
        ct <- getCurrentTime
        let diff = pointInFuture .-. ct
        threadDelay (fromIntegral (diff ^. microseconds))
        modifyMVar_ stateVar $ \(ProgramState db' dir _) -> do
          newWait <- newWaitJob db' stateVar uids
          for_ (formatEvents (eventsByUID db' uids)) sendMessage
          pure (ProgramState db' dir newWait)
      pure (Just (WaitJob backThread))

eventHandler :: MVar ProgramState -> Event -> IO ()
eventHandler stateVar event = modifyMVar_ stateVar (eventHandler' stateVar event)

eventHandler' :: MVar ProgramState -> Event -> ProgramState -> IO ProgramState
eventHandler' stateVar _ (ProgramState db dir wait) = do
  newDB <- eventDBFromFS dir
  for_ (formatDiffs (db `compareDB` newDB)) sendMessage
  for_ wait (killThread . getWaitTid)
  newWait <- newWaitJob newDB stateVar mempty
  pure (ProgramState newDB dir newWait)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  po <- readProgramOptions
  let dir = po ^. poDirectory
  db <- eventDBFromFS dir
  stateVar <- newEmptyMVar
  waitJob <- newWaitJob db stateVar mempty
  putMVar stateVar (ProgramState db dir waitJob)
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    TextIO.putStrLn "starting fs watch..."
    void $ watchTree
      mgr
      dir
      (const True)
      (eventHandler stateVar)
