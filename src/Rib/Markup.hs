{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Markup
  ( MarkupParser,
  )
where

import Path

type MarkupParser a = forall m b. MonadIO m => Path b File -> m (Either Text a)
