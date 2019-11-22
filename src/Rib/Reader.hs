{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Reader
  ( RibReader(..)
  , Article(..)
  )
where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text (Text)

import Lucid (Html)

-- | An article that is read from the source directory.
data Article doc = Article
  { _article_path :: FilePath
  , _article_doc :: doc
  -- TODO: If meta=Void works with aeson, we should remove the Maybe.
  , _article_meta :: Maybe Value
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
  readDoc :: FilePath -> Text -> Article doc
  -- TODO: Use index arguments (whatever its name is) to distinguish between the two FilePaths
  readDocIO :: FilePath -> FilePath -> IO (Article doc)
  renderDoc :: Article doc -> Html ()
