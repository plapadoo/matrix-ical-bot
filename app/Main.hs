module Main where

import           Config                         (Config, configBotUrl,
                                                 configIcalDir, configRoom)
import           Control.Applicative            (pure)
import           Control.Concurrent             (Chan, ThreadId, forkIO,
                                                 killThread, newChan, readChan,
                                                 threadDelay)
import           Control.Lens                   ((^.))
import           Control.Monad                  (void, when)
import           Data.AffineSpace               ((.-.))
import           Data.Bool                      (Bool (..))
import           Data.Foldable                  (for_)
import           Data.Function                  (const, ($))
import           Data.Maybe                     (Maybe (..))
import           Data.Monoid                    ((<>))
import           Data.String                    (IsString (fromString))
import qualified Data.Text.Lazy                 as LazyText
import           Data.Thyme.Clock               (getCurrentTime, microseconds)
import           Dhall                          (auto, input)
import           MatrixIcal                     (eventPath, latestEvent,
                                                 processIcalFile,
                                                 validEventPath)
import           MatrixIcalUtil                 (maybeJust)
import           Prelude                        (fromIntegral)
import           System.FSNotify                (Event (..), watchTreeChan,
                                                 withManager)
import           System.IO                      (IO, putStrLn)
import           Text.Show                      (show)
import           Web.Matrix.Bot.API             (sendMessage)
import           Web.Matrix.Bot.IncomingMessage (constructIncomingMessage)

processEvent :: Config -> Event -> IO ()
processEvent config event = do
  let mySendMessage m = void $ sendMessage (configBotUrl config) (configRoom config) m
  case event of
    Added fn _    -> processIcalFile fn mySendMessage "added"
    Modified fn _ -> processIcalFile fn mySendMessage "modified"
    Removed fn _  ->
      let message = constructIncomingMessage ("ical file “"<> fromString fn <>"” was removed") Nothing
      in mySendMessage message

newWaitJob :: Config -> IO (Maybe ThreadId)
newWaitJob config = do
  le <- latestEvent (configIcalDir config)
  maybeJust le (pure Nothing) $ \(pointInFuture, message) -> do
    backThread <- forkIO $ do
      ct <- getCurrentTime
      let diff = pointInFuture .-. ct
      putStrLn ("Waiting until " <> show pointInFuture)
      threadDelay (fromIntegral (diff ^. microseconds))
      void (sendMessage (configBotUrl config) (configRoom config) (constructIncomingMessage (LazyText.toStrict message) Nothing))
    pure (Just backThread)

main :: IO ()
main = do
  config <- input auto "/etc/matrix-bot/matrix-ical-bot.dhall"
  withManager $ \mgr -> do
    eventChan <- newChan

    -- start a watching job (in the background)
    _ <- watchTreeChan
      mgr
      (configIcalDir config)
      (const True)
      eventChan

    waitJob <- newWaitJob config
    mainLoop config eventChan waitJob

mainLoop :: Config -> Chan Event -> Maybe ThreadId -> IO ()
mainLoop config eventChan waitJob = do
  e <- readChan eventChan
  when (validEventPath (eventPath e)) (processEvent config e)
  for_ waitJob killThread
  newJob <- newWaitJob config
  mainLoop config eventChan newJob
