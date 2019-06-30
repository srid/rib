-- |

module Rib.Shake where

import qualified Data.ByteString.Char8 as BS8
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import System.Environment (withArgs)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Development.Shake (Action, Rebuild (..), Verbosity (Chatty), copyFileChanged, getDirectoryFiles, need,
                          readFile', shakeArgs, shakeOptions, shakeRebuild, shakeVerbosity, want, writeFile',
                          (%>), (|%>), (~>))
import Development.Shake.FilePath (dropDirectory1, dropExtension, (-<.>), (</>))

import Slick (jsonCache')

import Rib.Types

import qualified Settings


-- | convert 'build' filepaths into source file filepaths
destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

-- | convert a source file path into a URL
srcToURL :: FilePath -> String
srcToURL = ("/" ++) . dropDirectory1 . dropExtension

ribShake
  :: Bool
  -- ^ Force generate of requested targes
  -> Settings.Settings
  -- ^ Site settings
  -> IO ()
ribShake forceGen cfg = withArgs [] $ do
  -- ^ The withArgs above is to ensure that our own app arguments is not
  -- confusing Shake.
  let opts = shakeOptions
        { shakeVerbosity = Chatty
        , shakeRebuild = bool [] ((RebuildNow,) <$> Settings.rebuildPatterns cfg) forceGen
        }
  shakeArgs opts $ do
    -- TODO: Write my own jsonCache and stop depending on `Slick`
    getPostCached <- jsonCache' getPost

    want ["site"]

    -- Require all the things we need to build the whole site
    "site" ~>
      need ["static", "posts", Settings.destDir cfg </> "index.html"]

    -- Require all static assets
    "static" ~> do
      files <- getDirectoryFiles (Settings.contentDir cfg) $ Settings.staticFilePatterns cfg
      need $ (Settings.destDir cfg </>) <$> files

    -- Rule for handling static assets, just copy them from source to dest
    (Settings.destDir cfg </>) <$> Settings.staticFilePatterns cfg |%> \out ->
      copyFileChanged (destToSrc out) out

    -- Find and require every post to be built
    "posts" ~> do
      files <- getDirectoryFiles (Settings.contentDir cfg) $ Settings.postFilePatterns cfg
      need $ (Settings.destDir cfg </>) . (-<.> "html") <$> files

    -- build the main table of contents
    (Settings.destDir cfg </> "index.html") %> \out -> do
      files <- getDirectoryFiles (Settings.contentDir cfg) $ Settings.postFilePatterns cfg
      posts <- traverse (getPostCached . PostFilePath . (Settings.contentDir cfg </>)) files
      html <- liftIO $ Settings.renderPage cfg $ Page_Index posts
      writeFile' out $ BS8.unpack html

    -- rule for actually building posts
    (Settings.destDir cfg </> "*.html") %> \out -> do
      post <- getPostCached $ PostFilePath $ destToSrc out -<.> "md"
      html <- liftIO $ Settings.renderPage cfg $ Page_Post post
      writeFile' out $ BS8.unpack html

  where
    -- | Read and parse a Markdown post
    getPost :: PostFilePath -> Action Post
    getPost (PostFilePath postPath) = do
      let srcPath = destToSrc postPath -<.> "md"
      content <- T.decodeUtf8 . BS8.pack <$> readFile' srcPath
      let doc = Settings.parsePage cfg content
          postURL = T.pack $ srcToURL postPath
      pure $ Post doc postURL
