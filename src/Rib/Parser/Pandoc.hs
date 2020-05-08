{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Helpers for working with Pandoc documents
module Rib.Parser.Pandoc
  ( -- * Parsing
    parse,
    parsePure,

    -- * Rendering
    render,
    renderPandocInlines,

    -- * Extracting information
    extractMeta,
    getH1,
    getToC,
    getFirstImg,

    -- * Re-exports
    Pandoc,
    module Text.Pandoc.Readers,
  )
where

import Control.Monad.Except (MonadError, liftEither, runExcept)
import Data.Aeson
import Development.Shake (Action, readFile')
import Lucid (HtmlT, toHtmlRaw)
import Relude
import Rib.Shake (ribInputDir)
import System.FilePath
import Text.Pandoc
import qualified Text.Pandoc.Readers
import Text.Pandoc.Walk (query, walkM)
import Text.Pandoc.Writers.Shared (toTableOfContents)

-- | Pure version of `parse`
parsePure ::
  (ReaderOptions -> Text -> PandocPure Pandoc) ->
  Text ->
  Pandoc
parsePure textReader s =
  either (error . show) id $ runExcept $ do
    runPure' $ textReader readerSettings s

-- | Parse a lightweight markup language using Pandoc
parse ::
  -- | The pandoc text reader function to use, eg: `readMarkdown`
  (ReaderOptions -> Text -> PandocIO Pandoc) ->
  FilePath ->
  Action Pandoc
parse textReader f =
  either fail pure =<< do
    inputDir <- ribInputDir
    content <- toText <$> readFile' (inputDir </> f)
    fmap (first show) $ runExceptT $ do
      runIO' $ textReader readerSettings content

-- | Render a Pandoc document to HTML
render :: Monad m => Pandoc -> HtmlT m ()
render doc =
  either error id $ first show $ runExcept $ do
    runPure'
    $ fmap toHtmlRaw
    $ writeHtml5String writerSettings doc

-- | Extract the Pandoc metadata as JSON value
extractMeta :: Pandoc -> Maybe (Either Text Value)
extractMeta (Pandoc meta _) = flattenMeta meta

runPure' :: MonadError PandocError m => PandocPure a -> m a
runPure' = liftEither . runPure

runIO' :: (MonadError PandocError m, MonadIO m) => PandocIO a -> m a
runIO' = liftEither <=< liftIO . runIO

-- | Render a list of Pandoc `Text.Pandoc.Inline` values as Lucid HTML
--
-- Useful when working with `Text.Pandoc.Meta` values from the document metadata.
renderPandocInlines :: Monad m => [Inline] -> HtmlT m ()
renderPandocInlines =
  renderPandocBlocks . pure . Plain

renderPandocBlocks :: Monad m => [Block] -> HtmlT m ()
renderPandocBlocks =
  toHtmlRaw . render . Pandoc mempty

-- | Get the top-level heading as Lucid HTML
getH1 :: Monad m => Pandoc -> Maybe (HtmlT m ())
getH1 (Pandoc _ bs) = fmap renderPandocInlines $ flip query bs $ \case
  Header 1 _ xs -> Just xs
  _ -> Nothing

-- | Get the document table of contents
getToC :: Monad m => Pandoc -> HtmlT m ()
getToC (Pandoc _ bs) = renderPandocBlocks [toc]
  where
    toc = toTableOfContents writerSettings bs

-- | Get the first image in the document if one exists
getFirstImg ::
  Pandoc ->
  -- | Relative URL path to the image
  Maybe Text
getFirstImg (Pandoc _ bs) = listToMaybe $ flip query bs $ \case
  Image _ _ (url, _) -> [toText url]
  _ -> []

exts :: Extensions
exts =
  mconcat
    [ extensionsFromList
        [ Ext_yaml_metadata_block,
          Ext_fenced_code_attributes,
          Ext_auto_identifiers,
          Ext_smart
        ],
      githubMarkdownExtensions
    ]

readerSettings :: ReaderOptions
readerSettings = def {readerExtensions = exts}

writerSettings :: WriterOptions
writerSettings = def {writerExtensions = exts}

-- Internal code

-- | Flatten a Pandoc 'Meta' into a well-structured JSON object.
--
-- Renders Pandoc text objects into plain strings along the way.
flattenMeta :: Meta -> Maybe (Either Text Value)
flattenMeta (Meta meta) = fmap toJSON . traverse go <$> guarded (not . null) meta
  where
    go :: MetaValue -> Either Text Value
    go (MetaMap m) = toJSON <$> traverse go m
    go (MetaList m) = toJSONList <$> traverse go m
    go (MetaBool m) = pure $ toJSON m
    go (MetaString m) = pure $ toJSON m
    go (MetaInlines m) =
      bimap show toJSON
        $ runPure . plainWriter
        $ Pandoc mempty [Plain m]
    go (MetaBlocks m) =
      bimap show toJSON
        $ runPure . plainWriter
        $ Pandoc mempty m
    plainWriter = writePlain def
