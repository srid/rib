{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- Sensible defaults for writing the most simple static site
module Rib.Simple
  ( Page(..)
  , Post(..)
  , simpleBuildRules
  , settings
  ) where

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Text as T

import Development.Shake
import Development.Shake.FilePath ((-<.>), (</>))
import Lucid
import Text.Pandoc (Pandoc, readMarkdown, runPure)

import Rib.Pandoc (markdownReaderOptions)
import Rib.Server (getHTMLFileUrl)
import qualified Rib.Settings as S
import Rib.Shake (parsePandocCached)

-- TODO: Move these and simpleBuildRules to Rib.Simple
-- | Represents a HTML page that will be generated
data Page
  = Page_Index [Post]
  | Page_Post Post
  deriving (Generic, Show, FromJSON, ToJSON)

-- | A JSON serializable representation of a post's metadata
data Post = Post
  { _post_doc :: Pandoc
  , _post_url :: Text
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON)

-- Build rules for the simplest site possible.
--
-- Just posts and static files.
simpleBuildRules
  :: [FilePath]
  -- ^ Which files are considered to be static files.
  -> [FilePath]
  -- ^ Which files are considered to be post files
  -> S.Settings Page
  -> Action ()
simpleBuildRules staticFilePatterns postFilePatterns cfg@S.Settings {..} = do
  -- Copy static assets
  files <- getDirectoryFiles contentDir staticFilePatterns
  void $ forP files $ \inp ->
    copyFileChanged (contentDir </> inp) (destDir </> inp)

  -- Generate posts
  postFiles <- getDirectoryFiles contentDir postFilePatterns
  posts <- forP postFiles $ \f -> do
    let out = destDir </> f -<.> "html"
        inp = contentDir </> f
    doc <- parsePandocCached cfg inp
    let post = Post doc $ getHTMLFileUrl f
    liftIO $ renderToFile out $ pageWidget $ Page_Post post
    pure post

  -- Generate the main table of contents
  -- TODO: Support `draft` property
  liftIO $ renderToFile (destDir </> "index.html") $
    pageWidget $ Page_Index posts

settings :: S.Settings Page
settings = S.Settings
  { pageWidget = pre_ . toHtml . T.pack . show
  , parsePage = either (error . show) id . runPure . readMarkdown markdownReaderOptions

  , contentDir = "content"
  , destDir = "content.generated"
  , rebuildPatterns = ["**/*.html", "**/*.md"]

  , buildRules = simpleBuildRules ["static//**"] ["*.md"]
  }
