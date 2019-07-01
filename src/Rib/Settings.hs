{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Settings where

import Data.Default (Default (def))
import Data.Text (Text)
import qualified Data.Text as T

import Development.Shake.FilePath (FilePath)
import Reflex.Dom.Core
import Text.Pandoc (Extension (..), Pandoc, ReaderOptions, extensionsFromList, githubMarkdownExtensions,
                    readMarkdown, readerExtensions, runPure)

import Rib.Types (Page)


data Settings x = Settings
  { pageHTML :: Page -> StaticWidget x ()
  -- ^ Reflex widget for the page
  , parsePage :: Text -> Pandoc
  -- ^ Parse a text document like Markdown into Pandoc structure
  , contentDir :: FilePath
  -- ^ Directory name of the source content
  , destDir :: FilePath
  -- ^ Name of the directory where output files will be generated
  , staticFilePatterns :: [FilePath]
  -- ^ Which files are considered to be static files.
  , postFilePatterns :: [FilePath]
  -- ^ Which files are considered to be post files
  , rebuildPatterns :: [FilePath]
  -- ^ What to rebuild when --force is passed.
  --
  -- We rebuild only the post files, assuming html/css/md file parsing has
  -- changed in our Haskell source.
  }

instance Default (Settings x) where
  def = Settings
    { pageHTML = el "tt" . text . T.pack . show
    , parsePage = either (error . show) id . runPure . readMarkdown markdownOptions
    , contentDir = "content"
    , destDir = "content.generated"
    , staticFilePatterns = ["images//*"]
    , postFilePatterns = ["*.md"]
    , rebuildPatterns = ["**/*.html", "**/*.md"]
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
