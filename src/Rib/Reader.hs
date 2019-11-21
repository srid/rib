{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Reader
  ( RibReader(..)

  )
where

import Control.Monad.IO.Class
import Data.Text (Text)

import Text.Pandoc (Pandoc)
import Text.Pandoc.Readers (readMarkdown)
import Development.Shake (Action)
import Development.Shake.FilePath (FilePath)

import qualified Rib.Reader.Pandoc as Pandoc


class RibReader a where
  readDoc :: Text -> Action a

instance RibReader Pandoc where
  readDoc s = liftIO $ Pandoc.parse readMarkdown s
