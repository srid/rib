{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODo: What if I make this literate haskell thus blog post?
module Main where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Data.List (partition)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Development.Shake (Verbosity (Chatty), copyFileChanged, getDirectoryFiles, need, readFile', shakeArgs,
                          shakeOptions, shakeVerbosity, want, writeFile', (%>), (|%>), (~>))
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.FilePath (dropDirectory1, dropExtension, (-<.>), (</>))

import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import qualified Network.Wai.Handler.Warp as Warp
-- import Network.Wai.Parse
-- import Network.Wai.Middleware.CleanPath (cleanPath)
import System.Console.CmdArgs

-- import Reflex.Dom.Core hiding (def)

import Slick (compileTemplate', convert, jsonCache', markdownToHTML, substitute)

-- | The program will run in either of two modes
--
-- 1. Generate static files and exit immediately.
--
-- 1. Serve the generated static files, while automatically re-generating them
-- when the source files change.
data App
  = Serve {port :: Int}
  | Generate
  deriving (Data,Typeable,Show,Eq)

cli :: App
cli = modes
  [ Serve
      { port = 8080 &= help "Port to bind to"
      } &= help "Serve the generated site"
        &= auto  -- | Serve is the default command.
  , Generate
      &= help "Generate the site"
  ]


main :: IO ()
main = cmdArgs cli >>= \case
  Serve p ->
    Warp.run p $ staticApp $ defaultFileServerSettings "dist"
  Generate -> shakeArgs shakeOptions {shakeVerbosity = Chatty} $ do
    -- TODO: Understand how this works. The caching from Slick.
    getPostCached <- jsonCache' getPost

    want ["site"]

    -- Require all the things we need to build the whole site
    "site" ~>
      need ["static", "posts", "dist/index.html"]

    let staticFilePatterns = ["css//*", "js//*", "images//*"]
        -- ^ Which files are considered to be static files.
        postFilePatterns = ["*.md"]
        -- ^ Which files are considered to be post files

    -- Require all static assets
    "static" ~> do
      need . fmap ("dist" </>) =<< getDirectoryFiles "site" staticFilePatterns

    -- Rule for handling static assets, just copy them from source to dest
    ("dist" </>) <$> staticFilePatterns |%> \out ->
      copyFileChanged (destToSrc out) out

    -- Find and require every post to be built
    "posts" ~> do
      need . fmap (("dist" </>) . (-<.> "html")) =<< getDirectoryFiles "site" postFilePatterns

    -- build the main table of contents
    "dist/index.html" %> \out -> do
      posts <- traverse (getPostCached . PostFilePath . ("site" </>)) =<< getDirectoryFiles "site" postFilePatterns
      let indexInfo = uncurry IndexInfo $ partition ((== Just Programming) . category) posts
      writeFile' out =<< renderTemplate "site/templates/index.html" indexInfo

    -- rule for actually building posts
    "dist/*.html" %> \out -> do
      post <- getPostCached $ PostFilePath $ destToSrc out -<.> "md"
      writeFile' out =<< renderTemplate "site/templates/post.html" post

  where
    -- | Read and parse a Markdown post
    getPost (PostFilePath postPath) = do
      -- | Given a post source-file's file path as a cache key, load the Post object
      -- for it. This is used with 'jsonCache' to provide post caching.
      let srcPath = destToSrc postPath -<.> "md"
      postData <- markdownToHTML . T.pack =<< readFile' srcPath
      let postURL = T.pack $ srcToURL postPath
          withURL = _Object . at "url" ?~ Aeson.String postURL
          withSrc = _Object . at "srcPath" ?~ Aeson.String (T.pack srcPath)
      convert $ withSrc $ withURL postData

    -- | Render a mustache template with the given object
    -- TODO: Use reflex static renderer instead of mustache's compileTemplate'
    renderTemplate t o = do
      template <- compileTemplate' t
      pure $ T.unpack $ substitute template $ Aeson.toJSON o

-- | Represents the template dependencies of the index page
-- TODO: Represent category of posts generically. dependent-map?
data IndexInfo = IndexInfo
  { programming_posts :: [Post]
  , other_posts :: [Post]
  } deriving (Generic, Show)

instance FromJSON IndexInfo
instance ToJSON IndexInfo

data PostCategory
  = Programming
  | Other
  deriving (Generic, Show, Eq, Ord)

instance FromJSON PostCategory
instance ToJSON PostCategory

-- | A JSON serializable representation of a post's metadata
-- TODO: Use Text instead of String
data Post = Post
  { title :: String
  , description :: String
  , category :: Maybe PostCategory
  , content :: String
  , url :: String
  } deriving (Generic, Eq, Ord, Show)

instance FromJSON Post
instance ToJSON Post


-- A simple wrapper data-type which implements 'ShakeValue';
-- Used as a Shake Cache key to build a cache of post objects.
newtype PostFilePath = PostFilePath FilePath
  deriving (Show, Eq, Hashable, Binary, NFData)

-- | convert 'build' filepaths into source file filepaths
destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

-- | convert a source file path into a URL
srcToURL :: FilePath -> String
srcToURL = ("/" ++) . dropDirectory1 . dropExtension
