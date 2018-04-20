-- |Functions for constructing messages to the matrix bot
module IcalBot.MatrixMessage(
    MatrixMessage(..)
  , incomingMessageToText
  , plainMessage
  , messagePlainText
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

-- |A matrix.org message with a plain text and a HTML part
data MatrixMessage = MatrixMessage Text.Text (Maybe (Html ()))
                     deriving(Show)

messagePlainText :: MatrixMessage -> Text.Text
messagePlainText (MatrixMessage pt _) = pt

-- |Construct a plain-text matrix.org message from a string
plainMessageStr :: String -> MatrixMessage
plainMessageStr t = MatrixMessage (Text.pack t) Nothing

-- |Construct a plain-text matrix.org message from a text
plainMessage :: Text.Text -> MatrixMessage
plainMessage t = MatrixMessage t Nothing

-- |Convert the structure back to text, ready to be sent
incomingMessageToText :: MatrixMessage -> Text.Text
incomingMessageToText (MatrixMessage plain markup) = foldMap id (LazyText.toStrict . renderText . body_ <$> markup) <> plain
