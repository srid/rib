{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | TODO: Use this as the only exposed module for Rib?
module Rib where

import Data.Default (Default (def))
import qualified Data.Text as T

import Text.Pandoc
import Lucid

import Rib.Settings
import Rib.Shake

defaultSiteSettings :: Settings
defaultSiteSettings = Settings
  { pageWidget = pre_ . toHtml . T.pack . show
  , parsePage = either (error . show) id . runPure . readMarkdown markdownOptions

  , contentDir = "content"
  , destDir = "content.generated"
  , rebuildPatterns = ["**/*.html", "**/*.md"]

  , buildRules = simpleBuildRules ["static//**"] ["*.md"]
  }
  where
    -- | Reasonable options for reading a markdown file
    markdownOptions :: ReaderOptions
    markdownOptions = def { readerExtensions = exts }
     where
      exts = mconcat
        [ extensionsFromList
          [ Ext_yaml_metadata_block
          , Ext_fenced_code_attributes
          , Ext_auto_identifiers
          ]
        , githubMarkdownExtensions
        ]
