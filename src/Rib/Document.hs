{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Text.Show

-- | A document written in a lightweight markup language (LML)
--
-- The type variable `repr` indicates the representation type of the Markup
-- parser to be used.
data Document repr meta
  = Document
      { -- | Path to the document; relative to the source directory.
        _document_path :: Path Rel File,
        -- | Parsed representation of the document.
        _document_val :: repr,
        -- | HTML rendering of the parsed representation.
        _document_html :: Html (),
        -- | Metadata associated with the document as an aeson Value. If no metadata
        -- is provided this will be Nothing.
        _document_metaValue :: Maybe Value,
        -- | The parsed metadata.
        _document_meta :: meta
      }
  deriving (Generic, Show)

data DocumentError repr
  = DocumentError_MarkupError (MarkupError repr)
  | DocumentError_MetadataMissing
  | DocumentError_MetadataBadJSON String

instance Markup repr => Show (DocumentError repr) where
  show = \case
    DocumentError_MarkupError e -> toString (showMarkupError @repr e)
    DocumentError_MetadataMissing -> "Metadata missing"
    DocumentError_MetadataBadJSON msg -> "Bad metadata JSON: " <> msg

-- | Parse, render to HTML and extract metadata from the given file.
--
-- Return the Document type containing converted values.
mkDocumentFrom ::
  forall b repr meta.
  (Markup repr, FromJSON meta) =>
  -- | File path, used only to identify (not access) the document
  "relpath" :! Path Rel File ->
  -- | Actual file path, for access and reading
  "path" :! Path b File ->
  IO (Either (DocumentError repr) (Document repr meta))
mkDocumentFrom (Arg k) (Arg f) = runExceptT $ do
  -- HACK: this looks bad
  v :: repr <-
    liftEither
      =<< ( lift $ fmap (first DocumentError_MarkupError) $
              readDoc @repr @b
                ! #relpath k
                ! #path f
          )
  h <- withExceptT DocumentError_MarkupError $ liftEither $ renderDoc v
  let metaValueM = extractMeta v
  metaValue <- maybeToEither DocumentError_MetadataMissing metaValueM
  meta <-
    withExceptT DocumentError_MetadataBadJSON
      $ liftEither
      $ resultToEither
      $ fromJSON metaValue
  pure $ Document k v h metaValueM meta
  where
    maybeToEither e = \case
      Nothing -> throwError e
      Just v -> pure v
    resultToEither = \case
      Error e -> Left e
      Success v -> Right v
