{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Source
  ( -- * Source type
    Source,
    SourceReader,
    readSource,

    -- * Source properties
    sourcePath,
    sourceVal,
    sourceUrl,
  )
where

import Development.Shake.FilePath ((-<.>))
import Path hiding ((-<.>))

-- | A source file on disk
data Source repr
  = Source
      { -- | Path to the source; relative to the source directory.
        _source_path :: Path Rel File,
        -- | Parsed representation of the source.
        _source_val :: repr
      }
  deriving (Generic, Functor)

sourcePath :: Source repr -> Path Rel File
sourcePath = _source_path

sourceVal :: Source repr -> repr
sourceVal = _source_val

-- | Return the URL for the given @.html@ file under serve directory
--
-- File path must be relative to the serve directory.
--
-- You may also pass source paths as long as they map directly to destination
-- path except for file extension.
sourceUrl :: Source repr -> Text
sourceUrl doc = toText $ toFilePath ([absdir|/|] </> (sourcePath doc)) -<.> ".html"

-- | A function that parses a source representation out of the given file
type SourceReader repr =
  forall m b. MonadIO m => Path b File -> m (Either Text repr)

readSource ::
  MonadIO m =>
  SourceReader repr ->
  Path Rel File ->
  Path b File ->
  m (Either Text (Source repr))
readSource r k f = fmap (Source k) <$> r f
