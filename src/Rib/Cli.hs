{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Rib.Cli
  ( CliConfig (..),
    cliParser,

    -- * Internal
    hostPortParser,
  )
where

import Development.Shake (Verbosity (..))
import Options.Applicative
import Path
import Relude
import Relude.Extra.Tuple
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

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
        serve :: Maybe (Text, Int),
        -- | Shake's verbosity level.
        --
        -- Setting this to `Silent` will affect Rib's own logging as well.
        verbosity :: Verbosity,
        -- | Directory from which source content will be read.
        inputDir :: Path Abs Dir,
        -- | The path where static files will be generated.  Rib's server uses this
        -- directory when serving files.
        outputDir :: Path Abs Dir,
        -- | Path to shake's database directory.
        shakeDbDir :: Path Abs Dir
      }
  deriving (Show, Eq, Generic, Typeable)

cliParser :: Path Abs Dir -> Path Abs Dir -> Parser CliConfig
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
          (megaparsecReader hostPortParser)
          ( long "serve"
              <> short 's'
              <> metavar "[HOST]:PORT"
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
      option
        absDirReader
        ( long "input-dir"
            <> metavar "INPUTDIR"
            <> value inputDirDefault
            <> help ("Directory containing the source files (" <> "default: " <> toFilePath inputDirDefault <> ")")
        )
  outputDir <-
    option
      absDirReader
      ( long "output-dir"
          <> metavar "OUTPUTDIR"
          <> value outputDirDefault
          <> help ("Directory where files will be generated (" <> "default: " <> toFilePath outputDirDefault <> ")")
      )
  pure CliConfig {..}
  where
    absDirReader = eitherReader $ first (toString . displayException) . parseAbsDir

shakeDbDirFrom :: Path Abs Dir -> Path Abs Dir
shakeDbDirFrom inputDir =
  -- Keep shake database directory under the src directory instead of the
  -- (default) current working directory, which may not always be a project
  -- root (as in the case of neuron).
  inputDir </> [reldir|.shake|]

megaparsecReader :: M.Parsec Void Text a -> ReadM a
megaparsecReader p =
  eitherReader (first M.errorBundlePretty . M.parse p "<optparse-input>" . toText)

hostPortParser :: M.Parsec Void Text (Text, Int)
hostPortParser = do
  host <-
    optional $
      M.string "localhost"
        <|> M.try parseIP
  void $ M.char ':'
  port <- parseNumRange 1 65535
  pure (fromMaybe "127.0.0.1" host, port)
  where
    readNum = maybe (fail "Not a number") pure . readMaybe
    parseIP :: M.Parsec Void Text Text
    parseIP = do
      a <- parseNumRange 0 255 <* M.char '.'
      b <- parseNumRange 0 255 <* M.char '.'
      c <- parseNumRange 0 255 <* M.char '.'
      d <- parseNumRange 0 255
      pure $ toText $ intercalate "." $ show <$> [a, b, c, d]
    parseNumRange :: Int -> Int -> M.Parsec Void Text Int
    parseNumRange a b = do
      n <- readNum =<< M.some M.digitChar
      if a <= n && n <= b
        then pure n
        else fail $ "Number not in range: " <> show a <> "-" <> show b
