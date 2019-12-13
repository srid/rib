{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Rib.Document
  ( -- * Document type
    Document,
    SomeDoc (..),
    DocParser (..),
    mkDocumentFrom,

    -- * Document properties
    documentPath,
    documentVal,
    documentHtml,
    documentMeta,
    documentUrl,
  )
where

import Control.Monad.Except hiding (fail)
import Data.Aeson
import Development.Shake.FilePath ((-<.>))
import Lucid (Html)
import Named
import Path hiding ((-<.>))
import Rib.Markup
import Rib.Markup.MMark ()
-- TODO: for instances
import Rib.Markup.Pandoc ()
-- TODO: for instances

import Text.MMark (MMark)
import Text.Pandoc (Pandoc)
import qualified Text.Show

-- TODO: dsum, dmap?

data DocParser
  = DocParser_Pandoc
  | DocParser_MMark

data SomeDoc
  = SomeDoc_Pandoc Pandoc
  | SomeDoc_MMark MMark
  deriving (Show)

-- parseDoc' f s = \case
--   DocParser_Pandoc -> SomeDoc_Pandoc <$> parseDoc f s

readDoc' ::
  forall m b.
  MonadIO m =>
  -- | File path, used to identify the document only.
  "relpath" :! Path Rel File ->
  -- | Actual path to the file to parse.
  "path" :! Path b File ->
  DocParser ->
  m (Either Text SomeDoc)
readDoc' r f = \case
  DocParser_Pandoc -> fmap SomeDoc_Pandoc <$> readDoc r f
  DocParser_MMark -> fmap SomeDoc_MMark <$> readDoc r f

renderDoc' :: SomeDoc -> Either Text (Html ())
renderDoc' = \case
  SomeDoc_Pandoc doc -> renderDoc doc
  SomeDoc_MMark doc -> renderDoc doc

extractMeta' :: SomeDoc -> Maybe (Either Text Value)
extractMeta' = \case
  SomeDoc_Pandoc doc -> extractMeta doc
  SomeDoc_MMark doc -> extractMeta doc

-- | A document written in a lightweight markup language (LML)
--
-- The type variable `repr` indicates the representation type of the Markup
-- parser to be used.
data Document meta
  = Document
      { -- | Path to the document; relative to the source directory.
        _document_path :: Path Rel File,
        -- | Parsed representation of the document.
        _document_val :: SomeDoc,
        -- | HTML rendering of the parsed representation.
        _document_html :: Html (),
        -- | The parsed metadata.
        _document_meta :: meta
      }
  deriving (Generic, Show)

documentPath :: Document meta -> Path Rel File
documentPath = _document_path

documentVal :: Document meta -> SomeDoc
documentVal = _document_val

documentHtml :: Document meta -> Html ()
documentHtml = _document_html

documentMeta :: Document meta -> meta
documentMeta = _document_meta

-- | Return the URL for the given @.html@ file under serve directory
--
-- File path must be relative to the serve directory.
--
-- You may also pass source paths as long as they map directly to destination
-- path except for file extension.
documentUrl :: Document meta -> Text
documentUrl doc = toText $ toFilePath ([absdir|/|] </> (documentPath doc)) -<.> ".html"

data DocumentError
  = DocumentError_MarkupError Text
  | DocumentError_MetadataMissing
  | DocumentError_MetadataMalformed Text

instance Show DocumentError where
  show = \case
    DocumentError_MarkupError e -> toString e
    DocumentError_MetadataMissing -> "Metadata missing"
    DocumentError_MetadataMalformed msg -> "Bad metadata JSON: " <> toString msg

-- | Parse, render to HTML and extract metadata from the given file.
--
-- Return the Document type containing converted values.
mkDocumentFrom ::
  forall m b meta.
  (MonadError DocumentError m, MonadIO m, FromJSON meta) =>
  DocParser ->
  -- | File path, used only to identify (not access) the document
  "relpath" :! Path Rel File ->
  -- | Actual file path, for access and reading
  "path" :! Path b File ->
  m (Document meta)
mkDocumentFrom dp k@(arg #relpath -> k') f = do
  v <-
    liftEither . first DocumentError_MarkupError
      =<< readDoc' k f dp
  html <-
    liftEither . first DocumentError_MarkupError $
      renderDoc' v
  metaValue <-
    liftEither . (first DocumentError_MetadataMalformed)
      =<< maybeToEither DocumentError_MetadataMissing (extractMeta' v)
  meta <-
    liftEither . first (DocumentError_MetadataMalformed . toText) $
      resultToEither (fromJSON metaValue)
  pure $ Document k' v html meta
  where
    maybeToEither e = liftEither . maybeToRight e
    resultToEither = \case
      Error e -> Left e
      Success v -> Right v
