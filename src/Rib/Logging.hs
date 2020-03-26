{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logging utilities
module Rib.Logging
  ( -- * Basic
    formatPath,
    prettyPrintPathFile,
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

prettyPrintPathFile :: MonadThrow m => RibSettings -> Path b File -> m Text
prettyPrintPathFile RibSettings {..} fp =
  toText . toFilePath <$> makeRelative _ribSettings_workingDir fp

prettyPrintPathDir :: MonadThrow m => RibSettings -> Path b Dir -> m Text
prettyPrintPathDir RibSettings {..} fp =
  toText . toFilePath <$> makeRelative _ribSettings_workingDir fp

-- | Format a file path before printing it to the user
--
-- Returns the relative path to the base directory.
--
-- TODO: If the path is under the given base directory, return it as relative
-- to that base. Otherwise return the absolute path.
formatPath :: MonadThrow m => Path Abs Dir -> Path b File -> m Text
formatPath baseDir fp =
  toText . toFilePath <$> makeRelative baseDir fp
