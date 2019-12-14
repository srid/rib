{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
    Markup (..),
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
import Data.Dependent.Sum
import Data.Some
import Development.Shake.FilePath ((-<.>))
import Lucid (Html)
import Named
import Path hiding ((-<.>))
import Rib.Markup
import Rib.Markup.MMark ()
import Rib.Markup.Pandoc ()
import qualified Text.Show

-- | A document generated from a Markup source file.
data Document meta
  = Document
      { -- | Path to the document; relative to the source directory.
        _document_path :: Path Rel File,
        -- | Parsed representation of the document.
        _document_val :: DSum Markup Identity,
        -- | HTML rendering of the parsed representation.
        _document_html :: Html (),
        -- | The parsed metadata.
        _document_meta :: meta
      }
  deriving (Generic)

documentPath :: Document meta -> Path Rel File
documentPath = _document_path

documentVal :: Document meta -> DSum Markup Identity
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
  -- | Which Markup parser to use
  Some Markup ->
  -- | File path, used only to identify (not access) the document
  "relpath" :! Path Rel File ->
  -- | Actual file path, for access and reading
  "path" :! Path b File ->
  m (Document meta)
mkDocumentFrom mp k@(arg #relpath -> k') f = do
  v <-
    liftEither . first DocumentError_MarkupError
      =<< withSomeMarkup (readDoc k f) mp
  html <-
    liftEither . first DocumentError_MarkupError $
      withMarkup renderDoc v
  metaValue <-
    liftEither . (first DocumentError_MetadataMalformed)
      =<< maybeToEither DocumentError_MetadataMissing (withMarkup extractMeta v)
  meta <-
    liftEither . first (DocumentError_MetadataMalformed . toText) $
      resultToEither (fromJSON metaValue)
  pure $ Document k' v html meta
  where
    maybeToEither e = liftEither . maybeToRight e
    resultToEither = \case
      Error e -> Left e
      Success v -> Right v

withMarkup :: (forall doc. IsMarkup doc => doc -> a) -> DSum Markup Identity -> a
withMarkup f = \case
  Markup_Pandoc :=> Identity doc -> f doc
  Markup_MMark :=> Identity doc -> f doc

withSomeMarkup ::
  forall f f1.
  (Functor f, Functor f1) =>
  (forall doc. IsMarkup doc => f (f1 doc)) ->
  Some Markup ->
  f (f1 (DSum Markup Identity))
withSomeMarkup g = \case
  Some Markup_Pandoc -> fmap (Markup_Pandoc ==>) <$> g
  Some Markup_MMark -> fmap (Markup_MMark ==>) <$> g
