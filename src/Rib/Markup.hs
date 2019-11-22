{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Rib.Markup
  ( Markup(..)
  , Document(..)
  , getDocumentMeta
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Named

import Lucid (Html)

-- | A Markup document that is read from the source directory.
--
-- The type variable `t` indicates the type of parser to use.
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
  Nothing -> error $ "No metadata in document: " <> fp
  Just meta -> case fromJSON meta of
    Error e -> error e
    Success v -> v

class Markup t where
  type MarkupError t :: *
  readDoc :: FilePath -> Text -> Either (MarkupError t) (Document t)
  readDocIO :: "relpath" :! FilePath -> "path" :! FilePath -> IO (Either (MarkupError t) (Document t))
  renderDoc :: Document t -> Html ()
  showMarkupError :: MarkupError t -> Text
