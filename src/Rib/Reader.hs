{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Reader
  ( RibReader(..)
  , Document(..)
  )
where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text (Text)

import Lucid (Html)

data Document doc meta = Document
  { _document_path :: FilePath
  , _document_doc :: doc
  , _document_metadata :: Maybe meta
  }
  deriving (Generic, Show)

-- TODO: Consider `RibReader (doc, format) meta` for other Pandoc readers?
class FromJSON meta => RibReader doc meta where
  -- TODO: Should this be `Either Text a` to handle errors?
  -- TODO: Take FilePath (for showing in parser error)
  -- So just represent: (path, doc, meta)
  readDoc :: FilePath -> Text -> Document doc meta
  readDocIO :: FilePath -> FilePath -> IO (Document doc meta)
  renderDoc :: Document doc meta -> Html ()
