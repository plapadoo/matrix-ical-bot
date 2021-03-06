{-# LANGUAGE TemplateHaskell #-}

module ProgramOptions
  ( ProgramOptions(..)
  , readProgramOptions
  , poDirectory
  , poLogFile
  ) where

import           Control.Applicative    ((<$>), (<*>))
import           Control.Lens           (makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Monoid            ((<>))
import qualified Options.Applicative    as OptAppl
import           System.FilePath        (FilePath)

data ProgramOptions = ProgramOptions
  { _poDirectory :: FilePath
  , _poLogFile   :: FilePath
  }

makeLenses ''ProgramOptions

programOptionsParser :: OptAppl.Parser ProgramOptions
programOptionsParser =
  ProgramOptions <$>
    OptAppl.strOption
        (OptAppl.long "directory" <>
         OptAppl.help "The directory to watch") <*>
    OptAppl.strOption
        (OptAppl.long "log-file" <>
         OptAppl.help "The file to log into")

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
