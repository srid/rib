{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}

module Rib.Document
  ( -- * Document type
    Document,
    ParsedDoc (..),
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
-- TODO: for instances

-- TODO: for instances
import Data.Dependent.Sum
import Development.Shake.FilePath ((-<.>))
import Lucid (Html)
import Named
import Path hiding ((-<.>))
import Rib.Markup
import Rib.Markup.MMark ()
import Rib.Markup.Pandoc ()
import Text.MMark (MMark)
import Text.Pandoc (Pandoc)
import qualified Text.Show

data ParsedDoc doc where
  ParsedDoc_Pandoc :: ParsedDoc Pandoc
  ParsedDoc_MMark :: ParsedDoc MMark

withParsedDoc :: (forall doc. Markup doc => doc -> a) -> DSum ParsedDoc Identity -> a
withParsedDoc f = \case
  ParsedDoc_Pandoc :=> Identity doc -> f doc
  ParsedDoc_MMark :=> Identity doc -> f doc

-- | A document written in a lightweight markup language (LML)
--
-- The type variable `repr` indicates the representation type of the Markup
-- parser to be used.
data Document meta
  = Document
      { -- | Path to the document; relative to the source directory.
        _document_path :: Path Rel File,
        -- | Parsed representation of the document.
        _document_val :: DSum ParsedDoc Identity,
        -- | HTML rendering of the parsed representation.
        _document_html :: Html (),
        -- | The parsed metadata.
        _document_meta :: meta
      }
  deriving (Generic)

documentPath :: Document meta -> Path Rel File
documentPath = _document_path

documentVal :: Document meta -> DSum ParsedDoc Identity
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
  forall m b meta doc.
  (MonadError DocumentError m, MonadIO m, FromJSON meta, Markup doc) =>
  ParsedDoc doc ->
  -- | File path, used only to identify (not access) the document
  "relpath" :! Path Rel File ->
  -- | Actual file path, for access and reading
  "path" :! Path b File ->
  m (Document meta)
mkDocumentFrom dp k@(arg #relpath -> k') f = do
  v <-
    liftEither . first DocumentError_MarkupError
      =<< fmap (dp ==>) <$> readDoc k f
  html <-
    liftEither . first DocumentError_MarkupError $
      withParsedDoc renderDoc v
  metaValue <-
    liftEither . (first DocumentError_MetadataMalformed)
      =<< maybeToEither DocumentError_MetadataMissing (withParsedDoc extractMeta v)
  meta <-
    liftEither . first (DocumentError_MetadataMalformed . toText) $
      resultToEither (fromJSON metaValue)
  pure $ Document k' v html meta
  where
    maybeToEither e = liftEither . maybeToRight e
    resultToEither = \case
      Error e -> Left e
      Success v -> Right v
