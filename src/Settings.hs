{-# LANGUAGE Rank2Types #-}

module Settings where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS8
import Data.Default (Default (def))
import Data.Text (Text)

import Reflex.Dom.Core (StaticWidget, renderStatic)
import Text.Pandoc (Extension (..), Pandoc, ReaderOptions, extensionsFromList, githubMarkdownExtensions,
                    readMarkdown, readerExtensions, runPure)
import Development.Shake.FilePath (FilePath)

import Rib.Types (Page)

import HTML (pageWidget)


data Settings = Settings
  { renderPage :: Page -> IO BS8.ByteString
  -- ^ Render a page to HTML
  , parsePage :: Text -> Pandoc
  -- ^ Parse a text document like Markdown into Pandoc structure
  , contentDir :: FilePath
  -- ^ Directory of source content
  , destDir :: FilePath
  -- ^ Directory to generate
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

instance Default Settings where
  def = Settings
    { renderPage =
      liftIO . renderHTML . pageWidget
    , parsePage =
      either (error . show) id . runPure . readMarkdown markdownOptions
    , contentDir = "site"
    , destDir = "generated"
    , staticFilePatterns = ["images//*"]
    , postFilePatterns = ["*.md"]
    , rebuildPatterns = ["**/*.html", "**/*.md"]
    }
    where
      -- | Convert a Reflex DOM widget into HTML
      renderHTML :: StaticWidget x a -> IO BS8.ByteString
      renderHTML = fmap snd . renderStatic

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
