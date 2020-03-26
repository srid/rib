{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logging utilities
module Rib.Logging
  ( prettyPrintPathFile,
    prettyPrintPathDir,
    RibSettings (..),
  )
where

import Control.Monad.Catch
import Path
import Path.IO
import Relude

-- | RibSettings is initialized with the values passed to `Rib.App.run`
data RibSettings
  = RibSettings
      { _ribSettings_inputDir :: Path Abs Dir,
        _ribSettings_outputDir :: Path Abs Dir,
        _ribSettings_workingDir :: Path Abs Dir
      }
  deriving (Typeable)

prettyPrintPathFile :: (MonadIO m, MonadThrow m) => RibSettings -> Path b File -> m Text
prettyPrintPathFile RibSettings {..} fp = do
  fpAbs <- makeAbsolute fp
  if (toFilePath _ribSettings_workingDir) `isPrefixOf` (toFilePath fpAbs)
    then toText . toFilePath <$> makeRelative _ribSettings_workingDir fp
    else toText . toFilePath <$> pure fp

prettyPrintPathDir :: (MonadIO m, MonadThrow m) => RibSettings -> Path b Dir -> m Text
prettyPrintPathDir RibSettings {..} fp = do
  fpAbs <- makeAbsolute fp
  if (toFilePath _ribSettings_workingDir) `isPrefixOf` (toFilePath fpAbs)
    then toText . toFilePath <$> makeRelative _ribSettings_workingDir fp
    else toText . toFilePath <$> pure fp
