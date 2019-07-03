{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Rib.Shake
  ( ribShake
  ) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment (withArgs)

import Development.Shake (Action, Rebuild (..), Verbosity (Chatty), copyFileChanged, getDirectoryFiles, need,
                          readFile', shakeArgs, shakeOptions, shakeRebuild, shakeVerbosity, want, writeFile',
                          (%>), (|%>), (~>))
import Development.Shake.FilePath (dropDirectory1, dropExtension, (-<.>), (</>))
import Reflex.Dom.Core (renderStatic)
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
      (T.pack . ("/" ++) . dropDirectory1 . dropExtension)

    want ["site"]

    -- Require all the things we need to build the whole site
    "site" ~>
      need ["static", "posts", S.destDir cfg </> "index.html"]

    -- Require all static assets
    "static" ~> do
      files <- getDirectoryFiles (S.contentDir cfg) $ S.staticFilePatterns cfg
      need $ (S.destDir cfg </>) <$> files

    -- Rule for handling static assets, just copy them from source to dest
    (S.destDir cfg </>) <$> S.staticFilePatterns cfg |%> \out ->
      copyFileChanged (destToSrc out) out

    -- Find and require every post to be built
    "posts" ~> do
      files <- getDirectoryFiles (S.contentDir cfg) $ S.postFilePatterns cfg
      need $ (S.destDir cfg </>) . (-<.> "html") <$> files

    -- build the main table of contents
    (S.destDir cfg </> "index.html") %> \out -> do
      files <- getDirectoryFiles (S.contentDir cfg) $ S.postFilePatterns cfg
      posts <- traverse (getPostCached . PostFilePath . (S.contentDir cfg </>)) files
      writeReflexWidget out $ S.pageWidget cfg $ Page_Index posts

    -- rule for actually building posts
    (S.destDir cfg </> "*.html") %> \out -> do
      post <- getPostCached $ PostFilePath $ destToSrc out -<.> "md"
      writeReflexWidget out $ S.pageWidget cfg $ Page_Post post

  where
    writeReflexWidget out =
      writeFile' out <=< fmap (BS8.unpack . snd) . liftIO . renderStatic

    -- | Convert 'build' filepaths into source file filepaths
    -- FIXME: this assumes destDir setting
    destToSrc :: FilePath -> FilePath
    destToSrc = (S.contentDir cfg </>) . dropDirectory1

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
