{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Rib.Cli
  ( CliConfig (..),
    cliParser,
  )
where

import Development.Shake (Verbosity (..))
import Options.Applicative
import Path
import Relude

-- TODO: document
data CliConfig
  = CliConfig
      { rebuildAll :: Bool,
        watch :: Bool,
        serve :: Maybe Int,
        verbosity :: Verbosity,
        inputDir :: Path Rel Dir,
        outputDir :: Path Rel Dir,
        shakeDbDir :: Path Rel Dir
      }
  deriving (Show, Eq, Generic, Typeable)

cliParser :: Path Rel Dir -> Path Rel Dir -> Parser CliConfig
cliParser inputDirDefault outputDirDefault = do
  rebuildAll <- switch (long "rebuild-all" <> help "Rebuild all sources")
  watch <- switch (long "watch" <> short 'w' <> help "Watch for changes and regenerate")
  serve <-
    optional
      (option auto (long "serve" <> short 's' <> metavar "PORT" <> help "Run a HTTP server on the generated directory"))
  verbosity <- fmap (bool Verbose Silent) (switch (long "quiet" <> help "Log nothing"))
  inputDir <- relDirOption (long "input-dir" <> metavar "INPUTDIR" <> value (toFilePath inputDirDefault) <> help "Directory containing the source texts")
  outputDir <- relDirOption (long "output-dir" <> metavar "OUTPUTDIR" <> value (toFilePath outputDirDefault) <> help "Directory where files will be generated")
  -- Keep shake database directory under the src directory instead of the
  -- (default) current working directory, which may not always be a project
  -- root (as in the case of neuron).
  pure $ CliConfig rebuildAll watch serve verbosity inputDir outputDir (inputDir </> [reldir|.shake|])
  where
    relDirOption = fmap (either (error . toText . displayException) id . parseRelDir) . strOption
