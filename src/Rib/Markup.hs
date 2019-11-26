{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Rib.Markup
  ( -- * Type class
    Markup (..),

    -- * Document type
    Document (..),
    mkDocumentFrom,
  )
where

import Control.Monad.Except hiding (fail)
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
    Either (MarkupError repr) repr

  -- | Like `parseDoc` but take the actual filepath instead of text.
  readDoc ::
    forall b.
    -- | File path, used to identify the document only.
    "relpath" :! Path Rel File ->
    -- | Actual path to the file to parse.
    "path" :! Path b File ->
    IO (Either (MarkupError repr) repr)

  extractMeta ::
    repr ->
    Maybe Value

  -- | Render the document as Lucid HTML
  renderDoc ::
    repr ->
    Either (MarkupError repr) (Html ())

  -- | Convert `MarkupError` to string
  showMarkupError :: MarkupError repr -> Text
