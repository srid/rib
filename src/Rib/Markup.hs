{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- The type variable `t` indicates the type of Markup parser to use.
data Document t
  = Document
      { -- | Path to the document; relative to the source directory.
        _document_path :: Path Rel File,
        _document_val :: t,
        -- | Metadata associated with the document as an aeson Value. If no metadata
        -- is provided this will be Nothing.
        _document_meta :: Maybe Value
      }
  deriving (Generic, Show)

getDocumentMeta :: FromJSON meta => Document t -> meta
getDocumentMeta (Document fp _ mmeta) = case mmeta of
  Nothing -> error $ toText $ "No metadata in document: " <> toFilePath fp -- TODO: handle errors gracefully
  Just meta -> case fromJSON meta of
    Error e -> error $ toText e
    Success v -> v

-- | Markup class abstracts over the different markup libraries
--
-- See `Rib.Markup.Pandoc` and `Rib.Markup.MMark` for two available instances.
class Markup t where

  -- | Type representing parse errors
  type MarkupError t :: *

  -- | Parse the given markup text
  parseDoc ::
    -- | File path, used to identify the document only.
    Path Rel File ->
    -- | Markup text to parse
    Text ->
    Either (MarkupError t) (Document t)

  -- | Like `parseDoc` but take the actual filepath instead of text.
  readDoc ::
    -- | File path, used to identify the document only.
    "relpath" :! Path Rel File ->
    -- | Actual path to the file to parse.
    "path" :! Path b File ->
    IO (Either (MarkupError t) (Document t))

  -- | Render the document as Lucid HTML
  renderDoc :: Document t -> Html ()

  -- | Convert `MarkupError` to string
  showMarkupError :: MarkupError t -> Text
