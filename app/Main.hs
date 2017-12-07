module Main where

import           Config                         (Config, configBotUrl,
                                                 configIcalDir, configRoom)
import           Control.Concurrent             (newChan, readChan)
import           Control.Monad                  (forever, void, when)
import           Data.Bool                      (Bool (..))
import           Data.Function                  (const, ($))
import           Data.Maybe                     (Maybe (..))
import           Data.Monoid                    ((<>))
import           Data.String                    (IsString (fromString))
import           Dhall                          (auto, input)
import           MatrixIcal                     (eventPath, processIcalFile,
                                                 validEventPath)
import           Prelude                        ()
import           System.FSNotify                (Event (..), watchTreeChan,
                                                 withManager)
import           System.IO                      (IO)
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

    forever $ do
      event <- readChan eventChan
      when (validEventPath (eventPath event)) (processEvent config event)
