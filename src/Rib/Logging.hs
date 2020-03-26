{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logging utilities
module Rib.Logging
  ( prettyPrintPathFile,
    prettyPrintPathDir,
    RibSettings (..),
    withRelativeFileMaybe,
    withRelativeDirMaybe,
  )
where

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

prettyPrintPathFile :: (MonadIO m, Typeable b) => RibSettings -> Path b File -> m Text
prettyPrintPathFile s@RibSettings {..} fp = do
  withRelativeFileMaybe s fp $
    pure . toText . toFilePath

prettyPrintPathDir :: (MonadIO m, Typeable b) => RibSettings -> Path b Dir -> m Text
prettyPrintPathDir s@RibSettings {..} fp = do
  withRelativeDirMaybe s fp $
    pure . toText . toFilePath

withRelativeFileMaybe ::
  forall m b a.
  (MonadIO m, Typeable b) =>
  RibSettings ->
  Path b File ->
  (forall b1. Typeable b1 => Path b1 File -> m a) ->
  m a
withRelativeFileMaybe RibSettings {..} p f = do
  pAbs <- makeAbsolute p
  if (toFilePath _ribSettings_workingDir) `isPrefixOf` (toFilePath pAbs)
    then f =<< liftIO (makeRelative _ribSettings_workingDir p)
    else f p

withRelativeDirMaybe ::
  forall m b a.
  (MonadIO m, Typeable b) =>
  RibSettings ->
  Path b Dir ->
  (forall b1. Typeable b1 => Path b1 Dir -> m a) ->
  m a
withRelativeDirMaybe RibSettings {..} p f = do
  pAbs <- makeAbsolute p
  if (toFilePath _ribSettings_workingDir) `isPrefixOf` (toFilePath pAbs)
    then f =<< liftIO (makeRelative _ribSettings_workingDir p)
    else f p
