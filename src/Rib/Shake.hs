{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Rib.Shake
  ( ribShake
  , simpleBuildRules
  ) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment (withArgs)

import Development.Shake (Action, Rules, Rebuild (..), Verbosity (Chatty), copyFileChanged, getDirectoryFiles, need,
                          readFile', shakeArgs, shakeOptions, shakeRebuild, shakeVerbosity, want, writeFile',
                          (%>), (|%>), (~>))
import Development.Shake.FilePath (dropDirectory1, dropExtension, (-<.>), (</>))
import Reflex.Dom.Core (renderStatic, StaticWidget)
import Text.Pandoc (Pandoc)

import qualified Slick

import qualified Rib.Settings as S
import Rib.Types


ribShake
  :: Bool
  -- ^ Force generate of requested targes
  -> S.Settings x
  -- ^ Site settings
  -> IO ()
ribShake forceGen cfg = withArgs [] $ do
  -- The withArgs above is to ensure that our own app arguments is not
  -- confusing Shake.
  let opts = shakeOptions
        { shakeVerbosity = Chatty
        , shakeRebuild = bool [] ((RebuildNow,) <$> S.rebuildPatterns cfg) forceGen
        }
  shakeArgs opts $ do
    getPostCached <- Slick.jsonCache' $ getPost
      (S.parsePage cfg)
      -- TODO: This should be built from destination path, not source path.
      -- Such that it will work with serve.
      (T.pack . ("/" ++) . dropDirectory1 . dropExtension)

    runReaderT (S.buildRules cfg) (cfg, getPostCached)

-- Build rules for the simplest site possible.
--
-- Just posts and static files.
simpleBuildRules
  :: [FilePath]
  -- ^ Which files are considered to be static files.
  -> [FilePath]
  -- ^ Which files are considered to be post files
  -> ReaderT (S.Settings x, PostFilePath -> Action Post) Rules ()
simpleBuildRules staticFilePatterns postFilePatterns = do
  destDir <- asks $ S.destDir . fst
  contentDir <- asks $ S.contentDir . fst
  pageWidget <- asks $ S.pageWidget . fst
  getPostCached <- asks snd

  let
    -- | Convert 'build' filepaths into source file filepaths
    -- FIXME: this assumes destDir setting
    destToSrc :: FilePath -> FilePath
    destToSrc = (contentDir </>) . dropDirectory1

  lift $ do

    want ["site"]

    -- Require all the things we need to build the whole site
    "site" ~>
      need ["static", "posts", destDir </> "index.html"]

    -- Require all static assets
    "static" ~> do
      files <- getDirectoryFiles contentDir staticFilePatterns
      need $ (destDir </>) <$> files

    -- Rule for handling static assets, just copy them from source to dest
    (destDir </>) <$> staticFilePatterns |%> \out ->
      copyFileChanged (destToSrc out) out

    -- Find and require every post to be built
    "posts" ~> do
      files <- getDirectoryFiles contentDir postFilePatterns
      need $ (destDir </>) . (-<.> "html") <$> files

    -- build the main table of contents
    (destDir </> "index.html") %> \out -> do
      files <- getDirectoryFiles contentDir postFilePatterns
      -- TODO: Support `draft` property
      posts <- traverse (getPostCached . PostFilePath . (contentDir </>)) files
      writeReflexWidget out $ pageWidget $ Page_Index posts

    -- rule for actually building posts
    (destDir </> "*.html") %> \out -> do
      post <- getPostCached $ PostFilePath $ destToSrc out -<.> "md"
      writeReflexWidget out $ pageWidget $ Page_Post post


writeReflexWidget :: MonadIO m => FilePath -> StaticWidget x () -> m ()
writeReflexWidget out =
  writeFile' out <=< fmap (BS8.unpack . snd) . liftIO . renderStatic

-- Read and parse a Markdown post
getPost
  :: (Text -> Pandoc)
  -> (FilePath -> Text)
  -> PostFilePath
  -> Action Post
getPost parseContent mkPostUrl (PostFilePath postPath) = do
  content <- T.decodeUtf8 . BS8.pack <$> readFile' postPath
  let doc = parseContent content
  pure $ Post doc (mkPostUrl postPath)
