{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logging utilities
module Rib.Logging
  ( -- * Basic
    formatPath,
  )
where

import Control.Monad.Catch
import Path
import Path.IO
import Relude

-- | Format a file path before printing it to the user
--
-- Returns the relative path to the base directory.
--
-- TODO: If the path is under the given base directory, return it as relative
-- to that base. Otherwise return the absolute path.
formatPath :: MonadThrow m => Path Abs Dir -> Path b File -> m Text
formatPath baseDir fp =
  toText . toFilePath <$> makeRelative baseDir fp
