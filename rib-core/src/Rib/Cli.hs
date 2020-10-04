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
    Verbosity (..),

    -- * Parser helpers
    directoryReader,
    watchOption,
    serveOption,

    -- * Internal
    hostPortParser,
  )
where

import Development.Shake (Verbosity (..))
import Options.Applicative
import Relude
import Relude.Extra.Tuple
import System.FilePath
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
        inputDir :: FilePath,
        -- | The path where static files will be generated.  Rib's server uses this
        -- directory when serving files.
        outputDir :: FilePath,
        -- | Path to shake's database directory.
        shakeDbDir :: FilePath,
        -- | List of relative paths to ignore when watching the source directory
        watchIgnore :: [FilePath]
      }
  deriving (Show, Eq, Generic, Typeable)

cliParser :: FilePath -> FilePath -> Parser CliConfig
cliParser inputDirDefault outputDirDefault = do
  rebuildAll <-
    switch
      ( long "rebuild-all"
          <> help "Rebuild all sources"
      )
  watch <- watchOption
  serve <- serveOption
  verbosity <-
    fmap
      (bool Verbose Silent)
      ( switch
          ( long "quiet"
              <> help "Log nothing"
          )
      )
  ~(inputDir, shakeDbDir) <-
    fmap (toSnd shakeDbDirFrom) $
      option
        directoryReader
        ( long "input-dir"
            <> metavar "INPUTDIR"
            <> value inputDirDefault
            <> help ("Directory containing the source files (" <> "default: " <> inputDirDefault <> ")")
        )
  outputDir <-
    option
      directoryReader
      ( long "output-dir"
          <> metavar "OUTPUTDIR"
          <> value outputDirDefault
          <> help ("Directory where files will be generated (" <> "default: " <> outputDirDefault <> ")")
      )
  ~(watchIgnore) <- pure builtinWatchIgnores
  pure CliConfig {..}

watchOption :: Parser Bool
watchOption =
  switch
    ( long "watch"
        <> short 'w'
        <> help "Watch for changes and regenerate"
    )

serveOption :: Parser (Maybe (Text, Int))
serveOption =
  optional
    ( option
        (megaparsecReader hostPortParser)
        ( long "serve"
            <> short 's'
            <> metavar "[HOST]:PORT"
            <> help "Run a HTTP server on the generated directory"
        )
    )
    <|> ( fmap (bool Nothing $ Just (defaultHost, 8080)) $
            switch (short 'S' <> help ("Like `-s " <> toString defaultHost <> ":8080`"))
        )

builtinWatchIgnores :: [FilePath]
builtinWatchIgnores =
  [ ".shake",
    ".git"
  ]

shakeDbDirFrom :: FilePath -> FilePath
shakeDbDirFrom inputDir =
  -- Keep shake database directory under the src directory instead of the
  -- (default) current working directory, which may not always be a project
  -- root (as in the case of neuron).
  inputDir </> ".shake"

-- | Like `str` but adds a trailing slash if there isn't one.
directoryReader :: ReadM FilePath
directoryReader = fmap addTrailingPathSeparator str

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
  pure (fromMaybe defaultHost host, port)
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

defaultHost :: Text
defaultHost = "127.0.0.1"
