{-# LANGUAGE LambdaCase #-}

module Rib.Pandoc where

import Control.Monad
import Data.Aeson (FromJSON, decode)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Text.Pandoc
import Text.Pandoc.Highlighting
import Text.Pandoc.UTF8 (fromStringLazy)

-- Get the YAML metadata for the given key in a post.
--
-- We expect this to return `[Inline]` unless we upgrade pandoc. See
-- https://github.com/jgm/pandoc/issues/2139#issuecomment-310522113
getPandocMetaInlines :: String -> Pandoc -> Maybe [Inline]
getPandocMetaInlines k (Pandoc meta _) =
  case lookupMeta k meta of
    Just (MetaInlines inlines) -> Just inlines
    _ -> Nothing

-- Get the YAML metadata for a key that is a list of text values
getPandocMetaList :: String -> Pandoc -> Maybe [Text]
getPandocMetaList k (Pandoc meta _) =
  case lookupMeta k meta of
    Just (MetaList vals) -> Just $ catMaybes $ flip fmap vals $ \case
      MetaInlines [Str val] -> Just $ T.pack val
      _ -> Nothing
    _ -> Nothing

getPandocMetaRaw :: String -> Pandoc -> Maybe String
getPandocMetaRaw k p =
  getPandocMetaInlines k p >>= \case
    [Str v] -> Just v
    _ -> Nothing

-- Like getPandocMeta but expects the value to be JSON encoding of a type.
getPandocMetaJson :: FromJSON a => String -> Pandoc -> Maybe a
getPandocMetaJson k = decode . fromStringLazy <=< getPandocMetaRaw k

pandoc2Html' :: Pandoc -> Either PandocError Text
pandoc2Html' = runPure . writeHtml5String settings
  where
    settings :: WriterOptions
    settings = def

-- TODO: remove Monad and use error (like hakyll does)
pandoc2Html :: Monad m => Pandoc -> m Text
pandoc2Html = either (fail . show) pure . pandoc2Html'

pandocInlines2Html' :: [Inline] -> Either PandocError Text
pandocInlines2Html' = pandoc2Html . Pandoc mempty . pure . Plain

pandocInlines2Html :: Monad m => [Inline] -> m Text
pandocInlines2Html = either (fail . show) pure . pandocInlines2Html'

highlightingStyle :: Text
highlightingStyle = T.pack $ styleToCss tango
