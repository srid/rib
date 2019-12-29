{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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
    mkDocumentFrom,

    -- * Document properties
    documentPath,
    documentVal,
    documentUrl,
  )
where

import Control.Monad.Except hiding (fail)
import Development.Shake.FilePath ((-<.>))
import Named
import Path hiding ((-<.>))
import Rib.Markup
import Rib.Markup.MMark ()
import Rib.Markup.Pandoc ()
import qualified Text.Show

-- | A document generated from a Markup source file.
data Document repr
  = Document
      { -- | Path to the document; relative to the source directory.
        _document_path :: Path Rel File,
        -- | Parsed representation of the document.
        _document_val :: repr
      }
  deriving (Generic, Functor)

documentPath :: Document repr -> Path Rel File
documentPath = _document_path

documentVal :: Document repr -> repr
documentVal = _document_val

-- | Return the URL for the given @.html@ file under serve directory
--
-- File path must be relative to the serve directory.
--
-- You may also pass source paths as long as they map directly to destination
-- path except for file extension.
documentUrl :: Document repr -> Text
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
  forall m b repr.
  (MonadError DocumentError m, MonadIO m, IsMarkup repr) =>
  SubMarkup repr ->
  -- | File path, used only to identify (not access) the document
  "relpath" :! Path Rel File ->
  -- | Actual file path, for access and reading
  "path" :! Path b File ->
  m (Document repr)
mkDocumentFrom sm (arg #relpath -> k') f = do
  v <-
    liftEither . first DocumentError_MarkupError
      =<< readDoc sm f
  pure $ Document k' v
