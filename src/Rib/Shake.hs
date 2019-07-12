{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Rib.Shake
  ( ribShake
  , simpleBuildRules
  , parsePandocCached
  ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe
import qualified Data.Text.Encoding as T

import Development.Shake
import Development.Shake.FilePath ((-<.>), (</>))
import Development.Shake.Forward (cacheAction, shakeForward)
import Lucid
import Text.Pandoc (Pandoc)

import Rib.Server (getHTMLFileUrl)
import qualified Rib.Settings as S
import Rib.Types


ribShake
  :: Bool
  -- ^ Force generate of requested targes
  -> S.Settings
  -- ^ Site settings
  -> IO ()
ribShake forceGen cfg = do
  let opts = shakeOptions
        { shakeVerbosity = Chatty
        , shakeRebuild = bool [] ((RebuildNow,) <$> S.rebuildPatterns cfg) forceGen
        }
  shakeForward opts $
    S.buildRules cfg cfg

-- Build rules for the simplest site possible.
--
-- Just posts and static files.
simpleBuildRules
  :: [FilePath]
  -- ^ Which files are considered to be static files.
  -> [FilePath]
  -- ^ Which files are considered to be post files
  -> S.Settings
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

parsePandocCached :: S.Settings -> FilePath -> Action Pandoc
parsePandocCached cfg f =
  jsonCacheAction f $ parsePandoc $ S.parsePage cfg
  where
    jsonCacheAction k =
      fmap (fromMaybe (error "cache error") . decode) . cacheAction k . fmap encode
    parsePandoc parse =
      parse . T.decodeUtf8 . BS8.pack <$> readFile' f
