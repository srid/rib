{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Settings where

import Development.Shake
import Lucid (Html)

-- | Settings for building a static site.
data Settings page = Settings
  { renderPage :: page -> Html ()
  -- ^ Lucid widget for the page

  , parsePage :: FilePath -> Action page
  -- ^ Parse a text document like Markdown into Page structure

  , buildRules :: Settings page -> Action ()
  -- ^ Build rules specifying how to build the site
  }

type RibAction page = Action ()
