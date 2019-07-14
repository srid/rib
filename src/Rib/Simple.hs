{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- Sensible defaults for writing the most simple static site
module Rib.Simple
  ( Page(..)
  , Post(..)
  , isDraft
  , buildAction'
  , buildAction
  ) where

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Development.Shake
import Development.Shake.FilePath
import Lucid
import Text.Pandoc (Pandoc)

import Rib.App (ribOutputDir)
import Rib.Pandoc (getPandocMetaValue, parsePandoc)
import Rib.Server (getHTMLFileUrl)
import Rib.Shake (Action, jsonCacheAction)

-- | Represents a HTML page that will be generated
data Page
  = Page_Index [Post]
  | Page_Post Post
  deriving (Generic, Show, FromJSON, ToJSON)

-- | A Post corresponding to the Markdown content
data Post = Post
  { _post_doc :: Pandoc
  , _post_url :: Text
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON)

isDraft :: Post -> Bool
isDraft = fromMaybe False . getPandocMetaValue "draft" . _post_doc

buildAction :: (Page -> Html ()) -> Action ()
buildAction = buildAction' ["content/static//**"] ["content/*.md"]

-- Build rules for the simplest site possible.
--
-- Just posts and static files.
buildAction'
  :: [FilePath]
  -- ^ Which files are considered to be static files.
  -> [FilePath]
  -- ^ Which files are considered to be post files
  -> (Page -> Html ())
  -> Action ()
buildAction' staticFilePatterns postFilePatterns renderPage = do
  -- Copy static assets
  files <- getDirectoryFiles "." staticFilePatterns
  void $ forP files $ \inp ->
    copyFileChanged inp (ribOutputDir </> dropDirectory1 inp)

  -- Generate posts
  postFiles <- getDirectoryFiles "." postFilePatterns
  posts <- forP postFiles $ \f -> do
    let out = ribOutputDir </> dropDirectory1 f -<.> "html"
    Page_Post post <- jsonCacheAction f $ parsePage f
    liftIO $ renderToFile out $ renderPage $ Page_Post post
    pure post

  -- Generate the main table of contents
  let publicPosts = filter (not . isDraft) posts
  liftIO $ renderToFile (ribOutputDir </> "index.html") $
    renderPage $ Page_Index publicPosts
  where
    parsePage f = do
      doc <- parsePandoc . T.pack <$> readFile' f
      pure $ Page_Post $ Post doc $ getHTMLFileUrl $ dropDirectory1 f
