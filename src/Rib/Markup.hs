{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Rib.Markup
  ( Markup(..)
  , Document(..)
  , getDocumentMeta
  )
where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text (Text)

import Lucid (Html)

-- | A Markup document that is read from the source directory.
--
-- The type variable `t` indicates the type of parser to use.
data Document t = Document
  { _document_path :: FilePath
  , _document_val :: t
  , _document_meta :: Maybe Value
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
  -- TODO: rename to parseDoc
  readDoc :: FilePath -> Text -> Either (MarkupError t) (Document t)
  -- TODO: Use index arguments (whatever its name is) to distinguish between the two FilePaths
  readDocIO :: FilePath -> FilePath -> IO (Either (MarkupError t) (Document t))
  renderDoc :: Document t -> Html ()
  showMarkupError :: MarkupError t -> Text
