module Main where

import           Control.Applicative     (pure)
import           Control.Concurrent      (ThreadId, forkIO, killThread,
                                          threadDelay)
import           Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar,
                                          putMVar)
import           Control.Lens            ((^.))
import           Control.Monad           (forever, void)
import           Data.AffineSpace        ((.-.))
import           Data.Bool               (Bool (..))
import           Data.Foldable           (for_)
import           Data.Function           (on, (.))
import           Data.Function           (const, ($))
import           Data.List               (sortBy)
import           Data.Maybe              (Maybe (..))
import           Data.Monoid             ((<>))
import           Data.Ord                (compare)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as TextIO
import           Data.Thyme.Clock        (getCurrentTime, microseconds)
import           Data.Time.Zones         (loadTZFromDB)
import           Data.Tuple              (fst)
import           IcalBot.EventDB         (EventDB, compareDB, eventDBFromFS)
import           IcalBot.Formatting      (formatDiffs, textShow)
import           IcalBot.MatrixMessage   (MatrixMessage (..),
                                          incomingMessageToText)
import           IcalBot.Scheduling      (collectAppts, nextMessage)
import           IcalBot.Util            (foldMaybe')
import           Prelude                 (fromIntegral, (*), (+))
import           ProgramOptions          (ProgramOptions, poDirectory,
                                          poLogFile, readProgramOptions)
import           System.FilePath
import           System.FSNotify         (Event (..), watchTree, withManager)
import           System.IO               (BufferMode (NoBuffering), IO,
                                          hSetBuffering, stderr, stdout)

newtype WaitJob = WaitJob { getWaitTid :: ThreadId }

data ProgramState = ProgramState ProgramOptions EventDB FilePath (Maybe WaitJob)

sendMessage :: MatrixMessage -> IO ()
sendMessage = TextIO.putStrLn . incomingMessageToText

putErr :: ProgramOptions -> Text.Text -> IO ()
putErr po = TextIO.appendFile (po ^. poLogFile)

newWaitJob :: ProgramOptions -> EventDB -> MVar ProgramState -> IO (Maybe WaitJob)
newWaitJob po db stateVar = do
  ct <- getCurrentTime
  tz <- loadTZFromDB "Europe/Berlin"
  foldMaybe' (nextMessage db tz ct) $ \(pointInFuture, message) -> do
    putErr po ("Next message: " <> textShow pointInFuture <> ": " <> textShow message)
    putErr po ("Appts: " <> textShow (sortBy (compare `on` fst) (collectAppts db tz ct)))
    backThread <- forkIO $ do
      ct' <- getCurrentTime
      let diff = pointInFuture .-. ct'
          -- Add some seconds, hopefully to wake up _after_ the appointment
          waitTime = fromIntegral ((diff ^. microseconds) + 1000 * 1000 * 10)
      putErr po ("Waiting " <> textShow waitTime)
      threadDelay waitTime
      modifyMVar_ stateVar $ \(ProgramState po db' dir _) -> do
        newWait <- newWaitJob po db' stateVar
        sendMessage message
        pure (ProgramState po db' dir newWait)
    pure (WaitJob backThread)

eventHandler :: MVar ProgramState -> Event -> IO ()
eventHandler stateVar event = modifyMVar_ stateVar (eventHandler' stateVar event)

eventHandler' :: MVar ProgramState -> Event -> ProgramState -> IO ProgramState
eventHandler' stateVar _ (ProgramState po db dir wait) = do
  newDB <- eventDBFromFS dir
  tz <- loadTZFromDB "Europe/Berlin"
  for_ (formatDiffs tz (db `compareDB` newDB)) sendMessage
  for_ wait (killThread . getWaitTid)
  newWait <- newWaitJob po newDB stateVar
  pure (ProgramState po newDB dir newWait)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  po <- readProgramOptions
  let dir = po ^. poDirectory
  db <- eventDBFromFS dir
  stateVar <- newEmptyMVar
  waitJob <- newWaitJob po db stateVar
  putMVar stateVar (ProgramState po db dir waitJob)
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    putErr po "starting fs watch..."
    void $ watchTree
      mgr
      dir
      (const True)
      (eventHandler stateVar)

    forever (threadDelay 100000)
