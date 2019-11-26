{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Rib.Markup
  ( -- * Type class
    Markup (..),

    -- * Document type
    Document (..),
    getDocumentMeta,
  )
where

import Data.Aeson
import Lucid (Html)
import Named
import Path

-- | A document written in a lightweight markup language (LML)
--
-- The type variable `repr` indicates the representation type of the Markup
-- parser to be used.
data Document repr
  = Document
      { -- | Path to the document; relative to the source directory.
        _document_path :: Path Rel File,
        -- | Parsed representation of the document.
        _document_val :: repr,
        -- | HTML rendering of the parsed representation.
        _document_html :: Html (),
        -- | Metadata associated with the document as an aeson Value. If no metadata
        -- is provided this will be Nothing.
        _document_meta :: Maybe Value
      }
  deriving (Generic, Show)

getDocumentMeta :: FromJSON meta => Document repr -> meta
getDocumentMeta (Document fp _ _ mmeta) = case mmeta of
  Nothing -> error $ toText $ "No metadata in document: " <> toFilePath fp -- TODO: handle errors gracefully
  Just meta -> case fromJSON meta of
    Error e -> error $ toText e
    Success v -> v

-- | Class for denoting Markup representations.
--
-- See `Rib.Markup.Pandoc` and `Rib.Markup.MMark` for two available instances.
class Markup repr where

  -- | Type representing errors associated with parsing to, and rendering from,
  -- this representation.
  type MarkupError repr :: *

  -- | Parse the given markup text
  parseDoc ::
    -- | File path, used to identify the document only.
    Path Rel File ->
    -- | Markup text to parse
    Text ->
    Either (MarkupError repr) (Document repr)

  -- | Like `parseDoc` but take the actual filepath instead of text.
  readDoc ::
    -- | File path, used to identify the document only.
    "relpath" :! Path Rel File ->
    -- | Actual path to the file to parse.
    "path" :! Path b File ->
    IO (Either (MarkupError repr) (Document repr))

  -- | Render the document as Lucid HTML
  renderDoc ::
    Document repr ->
    Either (MarkupError repr) (Html ())

  -- | Convert `MarkupError` to string
  showMarkupError :: MarkupError repr -> Text
