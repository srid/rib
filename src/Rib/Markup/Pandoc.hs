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
  ( module Text.Pandoc.Readers,

    -- * Manual rendering
    renderPandoc,

    -- * Extracting information
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
import Relude.Extra.Map ((!?))
import Rib.Markup
import Text.Pandoc
import Text.Pandoc.Filter.IncludeCode (includeCode)
import Text.Pandoc.Readers
import Text.Pandoc.Walk (query, walkM)
import qualified Text.Show

data RibPandocError
  = RibPandocError_PandocError PandocError
  | RibPandocError_UnknownFormat UnknownExtension

instance Show RibPandocError where
  show = \case
    RibPandocError_PandocError e ->
      show e
    RibPandocError_UnknownFormat s ->
      "Unsupported extension: " <> show s

instance Markup Pandoc where

  type MarkupError Pandoc = RibPandocError

  parseDoc k s = runExcept $ do
    r <-
      withExcept RibPandocError_UnknownFormat
        $ liftEither
        $ detectReader k
    withExcept RibPandocError_PandocError $
      mkDoc k
        <$> parsePure r s

  readDoc (Arg k) (Arg f) = runExceptT $ do
    content <- readFileText $ toFilePath f
    r <-
      withExceptT RibPandocError_UnknownFormat $
        detectReader k
    withExceptT RibPandocError_PandocError $
      mkDoc k
        <$> parse r content

  renderDoc = render . _document_val

  showMarkupError = toText @String . show

-- | Parse and render the markup directly to HTML
renderPandoc :: Path Rel File -> Text -> Html ()
renderPandoc f =
  renderDoc
    . either (error . showMarkupError @Pandoc) id
    . parseDoc @Pandoc f

-- | Pure version of `parse`
parsePure ::
  MonadError PandocError m =>
  (ReaderOptions -> Text -> PandocPure Pandoc) ->
  Text ->
  m Pandoc
parsePure r =
  liftEither . runPure . r settings
  where
    settings = def {readerExtensions = exts}

-- | Parse the source text as a Pandoc document
--
-- Supports the [includeCode](https://github.com/owickstrom/pandoc-include-code) extension.
parse ::
  (MonadIO m, MonadError PandocError m) =>
  -- | Document format. Example: `Text.Pandoc.Readers.readMarkdown`
  (ReaderOptions -> Text -> PandocIO Pandoc) ->
  -- | Source text to parse
  Text ->
  m Pandoc
parse r s = do
  v' <- liftEither =<< liftIO (runIO $ r settings s)
  liftIO $ walkM includeSources v'
  where
    settings = def {readerExtensions = exts}
    includeSources = includeCode $ Just $ Format "html5"

-- | Like `render` but returns the raw HTML string, or the rendering error.
render' :: Pandoc -> Either PandocError Text
render' = runPure . writeHtml5String settings
  where
    settings = def {writerExtensions = exts}

-- | Render a Pandoc document as Lucid HTML
render :: Pandoc -> Html ()
render = either (error . show) toHtmlRaw . render'

-- | Like `renderInlines` but returns the raw HTML string, or the rendering error.
renderInlines' :: [Inline] -> Either PandocError Text
renderInlines' = render' . Pandoc mempty . pure . Plain

-- | Render a list of Pandoc `Text.Pandoc.Inline` values as Lucid HTML
--
-- Useful when working with `Text.Pandoc.Meta` values from the document metadata.
renderInlines :: [Inline] -> Html ()
renderInlines = either (error . show) toHtmlRaw . renderInlines'

-- | Get the top-level heading as Lucid HTML
getH1 :: Pandoc -> Maybe (Html ())
getH1 (Pandoc _ bs) = fmap renderInlines $ flip query bs $ \case
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

-- Internal code

data UnknownExtension
  = UnknownExtension String
  deriving (Show, Eq)

-- | Detect the Pandoc reader to use based on file extension
detectReader ::
  forall m m1.
  (MonadError UnknownExtension m, PandocMonad m1) =>
  Path Rel File ->
  m (ReaderOptions -> Text -> m1 Pandoc)
detectReader f = do
  ext <-
    catchInMonadError (UnknownExtension . show) $
      fileExtension f
  liftEither $ maybeToRight (UnknownExtension ext) $
    formats !? ext
  where
    -- TODO: This should compute using `Text.Pandoc.Readers.readers`
    formats :: Map String (ReaderOptions -> Text -> m1 Pandoc)
    formats =
      fromList
        [ (".md", readMarkdown),
          (".rst", readRST),
          (".org", readOrg),
          (".tex", readLaTeX)
        ]
    -- Re-constrain code constrained by MonadThrow to be constrained by
    -- MonadError instead.
    catchInMonadError ef = either (throwError . ef) pure

mkDoc :: Path Rel File -> Pandoc -> Document Pandoc
mkDoc f v = Document f v $ getMetadata v

getMetadata :: Pandoc -> Maybe Value
getMetadata (Pandoc meta _) = flattenMeta meta

-- | Flatten a Pandoc 'Meta' into a well-structured JSON object.
--
-- Renders Pandoc text objects into plain strings along the way.
flattenMeta :: Meta -> Maybe Value
flattenMeta (Meta meta) = toJSON . fmap go <$> guarded null meta
  where
    go :: MetaValue -> Value
    go (MetaMap m) = toJSON $ fmap go m
    go (MetaList m) = toJSONList $ fmap go m
    go (MetaBool m) = toJSON m
    go (MetaString m) = toJSON m
    go (MetaInlines m) = toJSON (runPure' . writer $ Pandoc mempty [Plain m])
    go (MetaBlocks m) = toJSON (runPure' . writer $ Pandoc mempty m)
    runPure' :: PandocPure a -> a
    runPure' = either (error . show) id . runPure
    writer = writePlain def
