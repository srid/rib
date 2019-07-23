{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Helpers for working with Pandoc documents
module Rib.Pandoc
  ( getMeta
  , setMeta
  , parse
  , parsePure
  , render
  , renderInlines
  , getH1
  )
where

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Lucid (Html, toHtmlRaw)
import Text.Pandoc
import Text.Pandoc.Filter.IncludeCode (includeCode)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (walkM)


class IsMetaValue a where
  parseMetaValue :: MetaValue -> a

instance IsMetaValue [Inline] where
  parseMetaValue = \case
    MetaInlines inlines -> inlines
    _ -> error "Not a MetaInline"

instance IsMetaValue (Html ()) where
  parseMetaValue = renderInlines . parseMetaValue @[Inline]

instance IsMetaValue Text where
  parseMetaValue = T.pack . stringify . parseMetaValue @[Inline]

instance {-# Overlappable #-} IsMetaValue a => IsMetaValue [a] where
  parseMetaValue = \case
    MetaList vals -> parseMetaValue <$> vals
    _ -> error "Not a MetaList"

-- NOTE: This requires UndecidableInstances, but is there a better way?
instance {-# Overlappable #-} Read a => IsMetaValue a where
  parseMetaValue = read . T.unpack . parseMetaValue @Text

-- | Get the metadata value for the given key in a Pandoc document.
--
-- It is recommended to call this function with TypeApplications specifying the
-- type of `a`.
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
setMeta :: Show a => String -> a -> Pandoc -> Pandoc
setMeta k v (Pandoc (Meta meta) bs) = Pandoc (Meta meta') bs
  where
    meta' = Map.insert k v' meta
    v' = MetaInlines [Str $ show v]

-- | Parse a pandoc document
parsePure :: Text -> Pandoc
parsePure =
  either (error . show) id . runPure . parseMarkdown

parse :: Text -> IO Pandoc
parse =
  either (error . show) (walkM includeSources) <=< runIO . parseMarkdown
  where
    includeSources = includeCode $ Just $ Format "html5"

parseMarkdown :: PandocMonad m => Text -> m Pandoc
parseMarkdown = readMarkdown settings
  where
    settings = def { readerExtensions = exts }

render' :: Pandoc -> Either PandocError Text
render' = runPure . writeHtml5String settings
  where
    settings = def { writerExtensions = exts }

-- | Render a Pandoc document as Lucid HTML
render :: Pandoc -> Html ()
render = either (error . show) toHtmlRaw . render'

renderInlines' :: [Inline] -> Either PandocError Text
renderInlines' = render' . Pandoc mempty . pure . Plain

-- | Render Pandoc inlines as Lucid HTML
renderInlines :: [Inline] -> Html ()
renderInlines = either (error . show) toHtmlRaw . renderInlines'

-- | Get the top-level heading as Lucid HTML
getH1 :: Pandoc -> Maybe (Html ())
getH1 (Pandoc _meta blocks) = listToMaybe $ catMaybes $ flip fmap blocks $ \case
  Header 1 _ xs -> Just $ renderInlines xs
  _ -> Nothing

exts :: Extensions
exts = mconcat
  [ extensionsFromList
    [ Ext_yaml_metadata_block
    , Ext_fenced_code_attributes
    , Ext_auto_identifiers
    , Ext_smart
    ]
  , githubMarkdownExtensions
  ]
