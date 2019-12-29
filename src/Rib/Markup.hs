{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Rib.Markup
  ( -- * Type class
    IsMarkup (..),
  )
where

import Named
import Path

-- | Class for denoting Markup representations.
--
-- See `Rib.Markup.Pandoc` and `Rib.Markup.MMark` for two available instances.
class IsMarkup repr where

  -- Rename the class and type to: IsDocument / MarkupType
  type SubMarkup repr :: *

  defaultSubMarkup :: SubMarkup repr

  -- | Parse the given markup text
  parseDoc ::
    SubMarkup repr ->
    -- | Markup text to parse
    Text ->
    Either Text repr

  -- | Like `parseDoc` but take the actual filepath instead of text.
  readDoc ::
    forall m b.
    MonadIO m =>
    SubMarkup repr ->
    -- | Actual path to the file to parse.
    "path" :! Path b File ->
    m (Either Text repr)
