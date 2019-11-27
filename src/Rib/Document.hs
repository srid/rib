{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Rib.Document
  ( -- * Document type
    Document (..),
    mkDocumentFrom,
  )
where

import Control.Monad.Except hiding (fail)
import Data.Aeson
import Lucid (Html)
import Named
import Path
import Rib.Markup
import qualified Text.Show

-- | A document written in a lightweight markup language (LML)
--
-- The type variable `repr` indicates the representation type of the Markup
-- parser to be used.
data Document repr meta
  = Document
      { -- | Path to the document; relative to the source directory.
        _document_path :: Path Rel File,
        -- | Parsed representation of the document.
        _document_val :: repr,
        -- | HTML rendering of the parsed representation.
        _document_html :: Html (),
        -- | Metadata associated with the document as an aeson Value. If no metadata
        -- is provided this will be Nothing.
        _document_metaValue :: Maybe Value,
        -- | The parsed metadata.
        _document_meta :: meta
      }
  deriving (Generic, Show)

data DocumentError
  = DocumentError_MarkupError Text
  | DocumentError_MetadataMissing
  | DocumentError_MetadataBadJSON Text

instance Show DocumentError where
  show = \case
    DocumentError_MarkupError e -> toString e
    DocumentError_MetadataMissing -> "Metadata missing"
    DocumentError_MetadataBadJSON msg -> "Bad metadata JSON: " <> toString msg

-- | Parse, render to HTML and extract metadata from the given file.
--
-- Return the Document type containing converted values.
mkDocumentFrom ::
  forall m b repr meta.
  (MonadError DocumentError m, MonadIO m, Markup repr, FromJSON meta) =>
  -- | File path, used only to identify (not access) the document
  "relpath" :! Path Rel File ->
  -- | Actual file path, for access and reading
  "path" :! Path b File ->
  m (Document repr meta)
mkDocumentFrom k@(arg #relpath -> k') f = do
  v <-
    liftEither
      . first DocumentError_MarkupError
        =<< liftIO (readDoc k f)
  html <-
    liftEither
      $ first DocumentError_MarkupError
      $ renderDoc v
  let metaValueM = extractMeta v
  metaValue <-
    maybeToEither
      DocumentError_MetadataMissing
      metaValueM
  meta <-
    liftEither
      $ first (DocumentError_MetadataBadJSON . toText)
      $ resultToEither
      $ fromJSON metaValue
  pure $ Document k' v html metaValueM meta
  where
    maybeToEither e = \case
      Nothing -> throwError e
      Just v -> pure v
    resultToEither = \case
      Error e -> Left e
      Success v -> Right v
