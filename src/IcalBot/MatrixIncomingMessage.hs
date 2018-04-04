module IcalBot.MatrixIncomingMessage(
    IncomingMessage(..)
  , incomingMessageToText
  , plainMessage
  , plainMessageStr) where

import           Data.Foldable  (foldMap)
import           Data.Function  (id, (.))
import           Data.Functor   ((<$>))
import           Data.Maybe     (Maybe (Nothing))
import           Data.Monoid    ((<>))
import           Data.String    (String)
import qualified Data.Text      as Text
import qualified Data.Text.Lazy as LazyText
import           Lucid          (Html, body_, renderText)
import           Text.Show      (Show)

data IncomingMessage = IncomingMessage Text.Text (Maybe (Html ()))
                     deriving(Show)

plainMessageStr :: String -> IncomingMessage
plainMessageStr t = IncomingMessage (Text.pack t) Nothing

plainMessage :: Text.Text -> IncomingMessage
plainMessage t = IncomingMessage t Nothing

incomingMessageToText :: IncomingMessage -> Text.Text
incomingMessageToText (IncomingMessage plain markup) = (foldMap id (LazyText.toStrict . renderText . body_ <$> markup)) <> plain
