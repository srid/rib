{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Rib.Markup
  (
  -- * Type class
    Markup(..)
  -- * Document type
  , Document(..)
  , getDocumentMeta
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Named

import Lucid (Html)

-- | A document written in a lightweight markup language (LML)
--
-- The type variable `t` indicates the type of Markup parser to use.
data Document t = Document
  { _document_path :: FilePath
  -- ^ Path to the document, always relative to the source directory.
  , _document_val :: t
  , _document_meta :: Maybe Value
  -- ^ Metadata associated with the document as an aeson Value. If no metadata
  -- is provided this will be Nothing.
  }
  deriving (Generic, Show)

getDocumentMeta :: FromJSON meta => Document t -> meta
getDocumentMeta (Document fp _ mmeta) = case mmeta of
  Nothing -> error $ "No metadata in document: " <> fp -- TODO: handle errors gracefully
  Just meta -> case fromJSON meta of
    Error e -> error e
    Success v -> v

-- | Markup class abstracts over the different markup libraries
--
-- See `Rib.Markup.Pandoc` and `Rib.Markup.MMark` for two available instances.
class Markup t where

  -- | Type representing parse errors
  type MarkupError t :: *

  -- | Parse the given markup text
  parseDoc
    :: FilePath
    -- ^ File path, used to identify the document only.
    -> Text
    -- ^ Markup text to parse
    -> Either (MarkupError t) (Document t)

  -- | Like `parseDoc` but take the actual filepath instead of text.
  readDoc
    :: "relpath" :! FilePath
    -- ^ File path, used to identify the document only.
    -> "path" :! FilePath
    -- ^ Actual path to the file to parse.
    -> IO (Either (MarkupError t) (Document t))

  -- | Render the document as Lucid HTML
  renderDoc :: Document t -> Html ()

  -- | Convert `MarkupError` to string
  showMarkupError :: MarkupError t -> Text
