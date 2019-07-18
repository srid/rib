{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Combinators for working with Shake
module Rib.Shake where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Binary
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Typeable

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward (cacheAction)
import Lucid
import Text.Pandoc (Pandoc)

import Rib.App (ribInputDir, ribOutputDir)
import Rib.Pandoc (parsePandoc)


-- | Shake action to copy static files as is
buildStaticFiles :: [FilePattern] -> Action [FilePath]
buildStaticFiles staticFilePatterns = do
  files <- getDirectoryFiles ribInputDir staticFilePatterns
  void $ forP files $ \f ->
    copyFileChanged (ribInputDir </> f) (ribOutputDir </> f)
  pure files

-- | Build multiple HTML files given a pattern of source files
--
-- Call `mkA` to create the final value given a file and its pandoc structure.
-- Return the list of final values used to render their HTMLs.
buildHtmlMulti
  :: [FilePattern]
  -- ^ Source file patterns
  -> ((FilePath, Pandoc) -> Html ())
  -> Action [(FilePath, Pandoc)]
buildHtmlMulti pat r = do
  xs <- readPandocMulti pat
  void $ forP xs $ \x ->
    buildHtml (fst x -<.> "html") (r x)
  pure xs

readPandocMulti :: [FilePattern] -> Action [(FilePath, Pandoc)]
readPandocMulti pat = do
  fs <- getDirectoryFiles ribInputDir pat
  forP fs $ \f ->
    jsonCacheAction f $ (f, ) <$> readPandoc f

readPandoc :: FilePath -> Action Pandoc
readPandoc =
    fmap (parsePandoc . T.decodeUtf8With T.lenientDecode . BSC.pack)
  . readFile'
  . (ribInputDir </>)

-- | Build a single HTML file with the given value
buildHtml :: FilePath -> Html () -> Action ()
buildHtml f html = do
  let out = ribOutputDir </> f
  writeHtml out html

writeHtml :: MonadIO m => FilePath -> Html () -> m ()
writeHtml f = liftIO . renderToFile f

-- | Like `Development.Shake.cacheAction` but uses JSON instance instead of Typeable / Binary on `b`.
jsonCacheAction :: (FromJSON b, Typeable k, Binary k, Show k, ToJSON a) => k -> Action a -> Action b
jsonCacheAction k =
    fmap (either error id . Aeson.eitherDecode)
  . cacheAction k
  . fmap Aeson.encode
