{-# LANGUAGE LambdaCase #-}

-- | Helpers for working with Pandoc documents
module Rib.Pandoc where

import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Lucid (Html, toHtmlRaw)
import Text.Pandoc
import Text.Pandoc.Highlighting (styleToCss, tango)

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

-- Like getPandocMetaRaw but expects the value to be of Haskell syntax
getPandocMetaValue :: Read a => String -> Pandoc -> Maybe a
getPandocMetaValue k doc = do
  s <- getPandocMetaRaw k doc
  pure $ fromMaybe (error $ "Invalid metadata value for key: " <> k) $ readMaybe s

-- | Get the YAML metadata, parsing it to Pandoc doc and then to HTML
getPandocMetaHTML :: String -> Pandoc -> Maybe (Html ())
getPandocMetaHTML k = fmap pandocInlines2Html . getPandocMetaInlines k

-- | Add, or set, a metadata data key to the given Haskell value
setPandocMetaValue :: Show a => String -> a -> Pandoc -> Pandoc
setPandocMetaValue k v (Pandoc (Meta meta) bs) = Pandoc (Meta meta') bs
  where
    meta' = Map.insert k v' meta
    v' = MetaInlines [Str $ show v]

data PostMeta = PostMeta
  { _postMeta_title :: Maybe [Inline]
  , _postMeta_draft :: Maybe Bool
  , _postMeta_next :: Maybe FilePath
  }
-- TODO: can I use dependent map here?
-- getPandocMeta :: Pandoc -> [(String, DSum a Identity)] -> DMap a Identity

pandoc2Html' :: Pandoc -> Either PandocError Text
pandoc2Html' = runPure . writeHtml5String settings
  where
    settings :: WriterOptions
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
