{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Settings where

import Development.Shake
import Lucid (Html)

-- | Settings for building a static site.
--
-- TODO: When settings change it should invalidate Shake cache. How do we do it?
data Settings page = Settings
  { pageWidget :: page -> Html ()
  -- ^ Lucid widget for the page

  , parsePage :: FilePath -> Action page
  -- ^ Parse a text document like Markdown into Page structure

  , contentDir :: FilePath
  -- ^ Directory name of the source content

  , destDir :: FilePath
  -- ^ Name of the directory where output files will be generated

  , rebuildPatterns :: [FilePath]
  -- ^ What to rebuild when --force is passed.
  --
  -- We rebuild only the post files, assuming html/css/md file parsing has
  -- changed in our Haskell source.

  , buildRules :: Settings page -> Action ()
  -- ^ Build rules specifying how to build the site
  }
