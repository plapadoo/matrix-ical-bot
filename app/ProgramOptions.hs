{-# LANGUAGE TemplateHaskell #-}

module ProgramOptions
  ( ProgramOptions(..)
  , readProgramOptions
  , poDirectory
  ) where

import           Control.Applicative    ((<$>), (<*>))
import           Control.Lens           (makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Monoid            ((<>))
import qualified Options.Applicative    as OptAppl
import           System.FilePath        (FilePath)

data ProgramOptions = ProgramOptions
  { _poDirectory  :: FilePath
  }

makeLenses ''ProgramOptions

programOptionsParser :: OptAppl.Parser ProgramOptions
programOptionsParser =
  ProgramOptions <$>
    OptAppl.strOption
        (OptAppl.long "directory" <>
         OptAppl.help "The directory to watch")

readProgramOptions
  :: MonadIO m
  => m ProgramOptions
readProgramOptions = liftIO (OptAppl.execParser opts)
  where
    opts =
      OptAppl.info
        (OptAppl.helper <*> programOptionsParser)
        (OptAppl.fullDesc <>
         OptAppl.progDesc
           "Listen for ical directory changes" <>
         OptAppl.header "matrix-ical - send ical calendar changes in a directory to matrix")
