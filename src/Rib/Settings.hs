{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Settings where

import Data.Text (Text)

import Development.Shake
import Lucid (Html)
import Text.Pandoc

import Rib.Types


-- | Settings for building a static site.
--
-- TODO: When settings change it should invalidate Shake cache. How do we do it?
data Settings = Settings
  { pageWidget :: Page -> Html ()
  -- ^ Lucid widget for the page

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

  , buildRules :: Settings -> Action ()
  -- ^ Build rules specifying how to build the site
  --
  -- A simple implementation is included, which you may copy over to customize
  -- your own version.
  --
  -- TODO: Better API than ReaderT of a tuple?
  }
