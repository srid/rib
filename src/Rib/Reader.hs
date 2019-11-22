{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Reader
  ( RibReader(..)
  )
where

import Data.Aeson
import Data.Text (Text)

import Lucid (Html)


class FromJSON meta => RibReader doc meta where
  -- TODO: Should this be `Either Text a` to handle errors?
  -- TODO: Take FilePath (for showing in parser error)
  -- So just represent: (path, doc, meta)
  readDoc :: Text -> (doc, Maybe meta)
  readDocIO :: Text -> IO (doc, Maybe meta)
  renderDoc :: (doc, Maybe meta) -> Html ()
  -- getMetadata :: FromJSON meta => doc -> Maybe meta

  readDocIO = pure . readDoc
