{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Settings where

import Data.Text (Text)
import Control.Monad.Reader

import Development.Shake
import Reflex.Dom.Core
import Text.Pandoc

import Rib.Types


-- | Settings for building a static site.
--
-- The `x` is used internally by Reflex to build a static widget.
-- TODO: When settings change it should invalidate Shake cache. How do we do it?
data Settings x = Settings
  { pageWidget :: Page -> StaticWidget x ()
  -- ^ Reflex widget for the page
  , parsePage :: Text -> Pandoc
  -- ^ Parse a text document like Markdown into Pandoc structure
  , contentDir :: FilePath
  -- ^ Directory name of the source content
  , destDir :: FilePath
  -- ^ Name of the directory where output files will be generated
  , rebuildPatterns :: [FilePath]
  -- ^ What to rebuild when --force is passed.
  --
  -- We rebuild only the post files, assuming html/css/md file parsing has
  -- changed in our Haskell source.
  -- ^ TODO: Better API than ReaderT of a tuple?
  , buildRules :: ReaderT (Settings x, PostFilePath -> Action Post) Rules ()
  }
