{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Rib.Shake where

import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment (withArgs)

import Development.Shake (Action, Rebuild (..), Verbosity (Chatty), copyFileChanged, getDirectoryFiles, need,
                          readFile', shakeArgs, shakeOptions, shakeRebuild, shakeVerbosity, want, writeFile',
                          (%>), (|%>), (~>))
import Development.Shake.FilePath (dropDirectory1, dropExtension, (-<.>), (</>))

import Slick (jsonCache')

import qualified Rib.Settings as S
import Rib.Types


-- | convert 'build' filepaths into source file filepaths
destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

-- | convert a source file path into a URL
srcToURL :: FilePath -> String
srcToURL = ("/" ++) . dropDirectory1 . dropExtension

ribShake
  :: Bool
  -- ^ Force generate of requested targes
  -> S.Settings
  -- ^ Site settings
  -> IO ()
ribShake forceGen cfg = withArgs [] $ do
  -- ^ The withArgs above is to ensure that our own app arguments is not
  -- confusing Shake.
  let opts = shakeOptions
        { shakeVerbosity = Chatty
        , shakeRebuild = bool [] ((RebuildNow,) <$> S.rebuildPatterns cfg) forceGen
        }
  shakeArgs opts $ do
    -- TODO: Write my own jsonCache and stop depending on `Slick`
    getPostCached <- jsonCache' getPost

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
      html <- liftIO $ S.pageHTML cfg $ Page_Index posts
      writeFile' out html

    -- rule for actually building posts
    (S.destDir cfg </> "*.html") %> \out -> do
      post <- getPostCached $ PostFilePath $ destToSrc out -<.> "md"
      html <- liftIO $ S.pageHTML cfg $ Page_Post post
      writeFile' out html

  where
    -- | Read and parse a Markdown post
    getPost :: PostFilePath -> Action Post
    getPost (PostFilePath postPath) = do
      let srcPath = destToSrc postPath -<.> "md"
      content <- T.decodeUtf8 . BS8.pack <$> readFile' srcPath
      let doc = S.parsePage cfg content
          postURL = T.pack $ srcToURL postPath
      pure $ Post doc postURL
