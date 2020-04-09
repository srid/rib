{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Cli
  ( CliConfig (..),
    cliParser,
  )
where

import Development.Shake (Verbosity (..))
import Options.Applicative
import Path
import Relude
import Relude.Extra.Tuple

-- Rib's CLI configuration
--
-- Can be retrieved using `Rib.Shake.getCliConfig` in the `Development.Shake.Action` monad.
data CliConfig
  = CliConfig
      { -- | Whether to rebuild all sources in Shake.
        rebuildAll :: Bool,
        -- | Whether to monitor `inputDir` for changes and re-generate
        watch :: Bool,
        -- | Whether to run a HTTP server on `outputDir`
        serve :: Maybe Int,
        -- | Shake's verbosity level.
        --
        -- Setting this to `Silent` will affect Rib's own logging as well.
        verbosity :: Verbosity,
        -- | Directory from which source content will be read.
        inputDir :: Path Rel Dir,
        -- | The path where static files will be generated.  Rib's server uses this
        -- directory when serving files.
        outputDir :: Path Rel Dir,
        -- | Path to shake's database directory.
        shakeDbDir :: Path Rel Dir
      }
  deriving (Show, Eq, Generic, Typeable)

cliParser :: Path Rel Dir -> Path Rel Dir -> Parser CliConfig
cliParser inputDirDefault outputDirDefault = do
  rebuildAll <-
    switch
      ( long "rebuild-all"
          <> help "Rebuild all sources"
      )
  watch <-
    switch
      ( long "watch"
          <> short 'w'
          <> help "Watch for changes and regenerate"
      )
  serve <-
    optional
      ( option
          auto
          ( long "serve"
              <> short 's'
              <> metavar "PORT"
              <> help "Run a HTTP server on the generated directory"
          )
      )
  verbosity <-
    fmap
      (bool Verbose Silent)
      ( switch
          ( long "quiet"
              <> help "Log nothing"
          )
      )
  ~(inputDir, shakeDbDir) <-
    fmap (mapToSnd shakeDbDirFrom) $
      relDirOption
        ( long "input-dir"
            <> metavar "INPUTDIR"
            <> value (toFilePath inputDirDefault)
            <> help ("Directory containing the source files (" <> "default: " <> toFilePath inputDirDefault <> ")")
        )
  outputDir <-
    relDirOption
      ( long "output-dir"
          <> metavar "OUTPUTDIR"
          <> value (toFilePath outputDirDefault)
          <> help ("Directory where files will be generated (" <> "default: " <> toFilePath outputDirDefault <> ")")
      )
  pure CliConfig {..}
  where
    relDirOption = fmap (either (error . toText . displayException) id . parseRelDir) . strOption

shakeDbDirFrom :: Path Rel Dir -> Path Rel Dir
shakeDbDirFrom inputDir =
  -- Keep shake database directory under the src directory instead of the
  -- (default) current working directory, which may not always be a project
  -- root (as in the case of neuron).
  inputDir </> [reldir|.shake|]
