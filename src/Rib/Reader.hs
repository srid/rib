{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Reader
  ( RibReader(..)
  )
where

import Data.Aeson
import Data.Text (Text)

import Lucid (Html)


class RibReader a where
  -- TODO: Should this be `Either Text a` to handle errors?
  readDoc :: Text -> a
  readDocIO :: Text -> IO a
  renderDoc :: a -> Html ()
  getMetadata :: FromJSON b => a -> Maybe b

  readDocIO = pure . readDoc
