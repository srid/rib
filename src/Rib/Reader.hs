{-# LANGUAGE TypeFamilies #-}
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

-- TODO: Fix naming.
--
-- Page, Post, Document ... oh my!
-- So:
-- Reserve "document" for underlying pandoc/mmark doc types.
-- Call this type, "Article"
-- And the output type, "Page" (Page_Index & Page_Article)
data Document doc = Document
  { _document_path :: FilePath
  , _document_doc :: doc
  -- TODO: If meta=Void works with aeson, we should remove the Maybe.
  , _document_metadata :: Maybe Value
  }
  deriving (Generic, Show)

-- TODO: Consider `RibReader (doc, format) meta` for other Pandoc readers?
-- TODO: If we replace meta with Value, we could eliminate MultiParamTypeClasses
-- However that prevents us from handling errors in the library.
class RibReader doc where
  -- TODO: Should this be `Either Text a` to handle errors?
  -- TODO: Take FilePath (for showing in parser error)
  -- So just represent: (path, doc, meta)
  -- TODO: rename to parseDoc
  -- TODO: This should take metadata as argument.
  readDoc :: FilePath -> Text -> Document doc
  -- TODO: Use index arguments (whatever its name is) to distinguish between the two FilePaths
  readDocIO :: FilePath -> FilePath -> IO (Document doc)
  renderDoc :: Document doc -> Html ()
