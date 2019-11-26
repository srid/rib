{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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

-- | Parse, render to HTML and extract metadata from the given file.
--
-- Return the Document type containing converted values.
mkDocumentFrom ::
  forall b repr.
  Markup repr =>
  -- | File path, used only to identify (not access) the document
  "relpath" :! Path Rel File ->
  -- | Actual file path, for access and reading
  "path" :! Path b File ->
  IO (Either (MarkupError repr) (Document repr))
mkDocumentFrom (Arg k) (Arg f) = runExceptT $ do
  -- HACK: this looks bad
  v :: repr <-
    liftEither
      =<< ( lift $
              readDoc @repr @b
                ! #relpath k
                ! #path f
          )
  let meta = extractMeta v
  h <- liftEither $ renderDoc v
  pure $ Document k v h meta
