{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Helpers for working with Pandoc documents
module Rib.Pandoc where

import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Lucid (Html, toHtmlRaw)
import Text.Pandoc
import Text.Pandoc.Highlighting (styleToCss, tango)
import Text.Pandoc.Shared (stringify)


class IsMetaValue a where
  parseMetaValue :: MetaValue -> a

instance {-# Overlaps #-} IsMetaValue [Inline] where
  parseMetaValue = \case
    MetaInlines inlines -> inlines
    _ -> error "Not a MetaInline"

instance IsMetaValue a => IsMetaValue [a] where
  parseMetaValue = \case
    MetaList vals -> parseMetaValue <$> vals
    _ -> error "Not a MetaList"

instance {-# Overlaps #-} IsMetaValue Text where
  parseMetaValue = T.pack . stringify . parseMetaValue @[Inline]

instance {-# Overlaps #-} IsMetaValue (Html ()) where
  parseMetaValue = pandocInlines2Html . parseMetaValue @[Inline]

-- XXX: This requires UndecidableInstances, but is there a better way?
instance Read a => IsMetaValue a where
  parseMetaValue = read . T.unpack . parseMetaValue @Text

-- | Get the metadata value for the given key in a Pandoc document.
--
-- `MetaValue` is parsed in accordance with the `IsMetaValue` class constraint.
-- Typical instances:
-- * `Html ()`: parse value as a Pandoc document and convert to Lucid Html
-- * `Text`: parse a raw value (Inline with one Str value)
-- * `[a]`: parse a list of values
-- * `Read a => a`: parse a raw value and then read it.
getMeta :: IsMetaValue a => String -> Pandoc -> Maybe a
getMeta k (Pandoc meta _) = parseMetaValue <$> lookupMeta k meta

-- | Add, or set, a metadata data key to the given Haskell value
-- TODO
setPandocMetaValue :: Show a => String -> a -> Pandoc -> Pandoc
setPandocMetaValue k v (Pandoc (Meta meta) bs) = Pandoc (Meta meta') bs
  where
    meta' = Map.insert k v' meta
    v' = MetaInlines [Str $ show v]

pandoc2Html' :: Pandoc -> Either PandocError Text
pandoc2Html' = runPure . writeHtml5String settings
  where
    settings = def { writerExtensions = ribExts }

pandoc2Html :: Pandoc -> Html ()
pandoc2Html = either (error . show) toHtmlRaw . pandoc2Html'

pandocInlines2Html' :: [Inline] -> Either PandocError Text
pandocInlines2Html' = pandoc2Html' . Pandoc mempty . pure . Plain

pandocInlines2Html :: [Inline] -> Html ()
pandocInlines2Html = either (error . show) toHtmlRaw . pandocInlines2Html'

pandocH1 :: Pandoc -> Maybe (Html ())
pandocH1 (Pandoc _meta blocks) = listToMaybe $ catMaybes $ flip fmap blocks $ \case
  Header 1 _ xs -> Just $ pandocInlines2Html xs
  _ -> Nothing

-- TODO: Do we need this?
highlightingCss :: Text
highlightingCss = T.pack $ styleToCss tango

parsePandoc :: Text -> Pandoc
parsePandoc = either (error . show) id . runPure . readMarkdown markdownReaderOptions

-- Reasonable options for reading a markdown file
markdownReaderOptions :: ReaderOptions
markdownReaderOptions = def { readerExtensions = ribExts }

ribExts :: Extensions
ribExts = mconcat
  [ extensionsFromList
    [ Ext_yaml_metadata_block
    , Ext_fenced_code_attributes
    , Ext_auto_identifiers
    , Ext_smart
    ]
  , githubMarkdownExtensions
  ]
