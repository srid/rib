{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Reader
  ( RibReader(..)

  )
where

import Data.Text (Text)

import Text.Pandoc (Pandoc)
import Text.Pandoc.Readers (readMarkdown)

import qualified Rib.Reader.Pandoc as Pandoc


class RibReader a where
  readDoc :: Text -> a
  readDocIO :: Text -> IO a

instance RibReader Pandoc where
  readDoc = Pandoc.parsePure readMarkdown
  readDocIO = Pandoc.parse readMarkdown
