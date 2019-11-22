{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Reader
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
  -- TODO: If meta=Void works with aeson, we should remove the Maybe.
  , _document_meta :: Maybe Value
  }
  deriving (Generic, Show)

getDocumentMeta :: FromJSON meta => Document t -> meta
getDocumentMeta (Document fp _ mmeta) = case mmeta of
  Nothing -> error $ "No metadata in document: " <> fp
  Just meta -> case fromJSON meta of
    Error e -> error e
    Success v -> v


-- TODO: Consider `Markup (doc, format)` for other Pandoc readers?
class Markup t where
  -- TODO: Should this be `Either Text (Document doc)` to handle errors?
  -- TODO: Take FilePath (for showing in parser error)
  -- So just represent: (path, doc)
  -- TODO: rename to parseDoc
  -- TODO: This should take metadata as argument.
  readDoc :: FilePath -> Text -> Document t
  -- TODO: Use index arguments (whatever its name is) to distinguish between the two FilePaths
  readDocIO :: FilePath -> FilePath -> IO (Document t)
  renderDoc :: Document t -> Html ()
