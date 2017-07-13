{-# LANGUAGE DeriveGeneric #-}
module Config(Config,configRoom,configBotUrl,configIcalDir) where

import GHC.Generics(Generic)
import Dhall(Interpret,Text)
import Data.Function((.))
import Data.Text.Buildable(build)
import Data.Text.Lazy.Builder(toLazyText)
import Data.Text.Lazy(toStrict)
import qualified Data.Text as Text
import Data.String(String)

data Config = Config {
    room :: Text
  , botUrl :: Text
  , icalDir :: Text
  } deriving(Generic)

instance Interpret Config

toText :: Text -> Text.Text
toText = toStrict . toLazyText . build

toString :: Text -> String
toString = Text.unpack . toText

configRoom :: Config -> Text.Text
configRoom = toText . room

configBotUrl :: Config -> Text.Text
configBotUrl = toText . botUrl

configIcalDir :: Config -> String
configIcalDir = toString . icalDir
