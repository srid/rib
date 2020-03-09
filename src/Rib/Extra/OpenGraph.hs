{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Meta tags for The Open Graph protocol: https://ogp.me/
module Rib.Extra.OpenGraph where

import Lucid
import Lucid.Base (makeAttribute)
import Relude
import qualified Text.URI as URI

data OpenGraph
  = OpenGraph
      { _openGraph_title :: Text,
        _openGraph_author :: Text,
        _openGraph_description :: Maybe Text,
        _openGraph_siteName :: Text,
        _openGraph_type :: Text, -- TODO: ADT
        _openGraph_image :: Maybe URI.URI
      }
  deriving (Eq, Show)

instance ToHtml OpenGraph where
  toHtmlRaw = toHtml
  toHtml OpenGraph {..} = do
    meta' "author" _openGraph_author
    meta' "description" `mapM_` _openGraph_description
    metaOg "title" _openGraph_title
    metaOg "site_name" _openGraph_siteName
    metaOg "type" _openGraph_type
    -- TODO: This cannot be relative URL. Perhaps we should enforce it?
    metaOg "image" `mapM_` fmap URI.render _openGraph_image
    where
      -- Open graph meta element
      metaOg k v =
        meta_
          [ makeAttribute "property" $ "og:" <> k,
            content_ v
          ]
      meta' k v = meta_ [name_ k, content_ v]
