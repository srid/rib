{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Suppressing orphans warning for `Markup Pandoc` instance

-- | Helpers for working with Pandoc documents
module Rib.Markup.Pandoc
  ( -- * Rendering
    render,
    renderPandocInlines,

    -- * Extracting information
    extractMeta,
    getH1,
    getFirstImg,

    -- * Re-exports
    Pandoc,
  )
where

import Control.Monad.Except
import Data.Aeson
import Lucid (Html, toHtmlRaw)
import Named
import Path
import Rib.Markup
import Text.Pandoc
import Text.Pandoc.Filter.IncludeCode (includeCode)
import Text.Pandoc.Walk (query, walkM)

-- | List of formats supported by Pandoc
--
-- TODO: Complete this list.
data PandocFormat
  = PandocFormat_Markdown
  | PandocFormat_RST

readPandocFormat :: PandocMonad m => PandocFormat -> ReaderOptions -> Text -> m Pandoc
readPandocFormat = \case
  PandocFormat_Markdown -> readMarkdown
  PandocFormat_RST -> readRST

instance IsMarkup Pandoc where

  type SubMarkup Pandoc = PandocFormat

  defaultSubMarkup = PandocFormat_Markdown

  parseDoc k s =
    first show $ runExcept $ do
      runPure'
      $ readPandocFormat k readerSettings s

  readDoc k (Arg f) = fmap (first show) $ runExceptT $ do
    content <- readFileText (toFilePath f)
    v' <- runIO' $ readPandocFormat k readerSettings content
    liftIO $ walkM includeSources v'
    where
      includeSources = includeCode $ Just $ Format "html5"

-- | Render a Pandoc document to HTML
render :: Pandoc -> Html ()
render doc =
  either error id $ first show $ runExcept $ do
    runPure'
    $ fmap toHtmlRaw
    $ writeHtml5String writerSettings doc

extractMeta :: Pandoc -> Maybe (Either Text Value)
extractMeta (Pandoc meta _) = flattenMeta meta

runPure' :: MonadError PandocError m => PandocPure a -> m a
runPure' = liftEither . runPure

runIO' :: (MonadError PandocError m, MonadIO m) => PandocIO a -> m a
runIO' = liftEither <=< liftIO . runIO

-- | Render a list of Pandoc `Text.Pandoc.Inline` values as Lucid HTML
--
-- Useful when working with `Text.Pandoc.Meta` values from the document metadata.
renderPandocInlines :: [Inline] -> Html ()
renderPandocInlines =
  toHtmlRaw
    . render
    . Pandoc mempty
    . pure
    . Plain

-- | Get the top-level heading as Lucid HTML
getH1 :: Pandoc -> Maybe (Html ())
getH1 (Pandoc _ bs) = fmap renderPandocInlines $ flip query bs $ \case
  Header 1 _ xs -> Just xs
  _ -> Nothing

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
flattenMeta (Meta meta) = fmap toJSON . traverse go <$> guarded null meta
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
