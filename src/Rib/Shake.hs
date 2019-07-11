{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Rib.Shake
  ( ribShake
  , simpleBuildRules
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import System.Environment (withArgs)

import Development.Shake (Action, Rebuild (..), Rules, Verbosity (Chatty), copyFileChanged, getDirectoryFiles,
                          need, readFile', shakeArgs, shakeOptions, shakeRebuild, shakeVerbosity, want, (%>),
                          (|%>), (~>))
import Development.Shake.FilePath (dropDirectory1, (-<.>), (</>))
import Lucid
import Text.Pandoc (Pandoc)

import qualified Slick

import Rib.Server (getHTMLFileUrl)
import qualified Rib.Settings as S
import Rib.Types


ribShake
  :: Bool
  -- ^ Force generate of requested targes
  -> S.Settings
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
    let parsePage = S.parsePage cfg
    parsePandocCached <- Slick.jsonCache' $ parsePandoc parsePage

    runReaderT (S.buildRules cfg) (cfg, parsePandocCached)

-- Build rules for the simplest site possible.
--
-- Just posts and static files.
simpleBuildRules
  :: [FilePath]
  -- ^ Which files are considered to be static files.
  -> [FilePath]
  -- ^ Which files are considered to be post files
  -> ReaderT (S.Settings, PostFilePath -> Action Pandoc) Rules ()
simpleBuildRules staticFilePatterns postFilePatterns = do
  destDir <- asks $ S.destDir . fst
  contentDir <- asks $ S.contentDir . fst
  pageWidget <- asks $ S.pageWidget . fst
  parsePandocCached <- asks snd

  let
    -- | Convert 'build' filepaths into source file filepaths
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

    -- Build the main table of contents
    (destDir </> "index.html") %> \out -> do
      files <- getDirectoryFiles contentDir postFilePatterns
      -- TODO: Support `draft` property
      posts <- forM files $ \f -> do
        doc <- parsePandocCached $ PostFilePath (contentDir </> f)
        pure $ Post doc $ getHTMLFileUrl f
      writeLucidHtml out $ pageWidget $ Page_Index posts

    -- Rule for building individual posts
    (destDir </> "*.html") %> \out -> do
      let f = dropDirectory1 $ destToSrc out -<.> "md"
      doc <- parsePandocCached $ PostFilePath (contentDir </> f)
      let post = Post doc $ getHTMLFileUrl f
      writeLucidHtml out $ pageWidget $ Page_Post post


-- | TODO: Put this in Shake monad?
writeLucidHtml :: MonadIO m => FilePath -> Html () -> m ()
writeLucidHtml out = liftIO . renderToFile out

-- Require the given post file and parse it as Pandoc document.
parsePandoc
  :: (Text -> Pandoc)
  -> PostFilePath
  -> Action Pandoc
parsePandoc parse (PostFilePath f) =
  parse . T.decodeUtf8 . BS8.pack <$> readFile' f
