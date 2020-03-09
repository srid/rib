{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Meta tags for The Open Graph protocol: https://ogp.me/
module Rib.Extra.OpenGraph
  ( OpenGraph (..),
  )
where

import Lucid
import Lucid.Base (makeAttribute)
import Relude
import qualified Text.URI as URI

-- TODO: This should be Haskell library of its own?

data OpenGraph
  = OpenGraph
      { _openGraph_title :: Text,
        _openGraph_author :: Text,
        _openGraph_description :: Maybe Text,
        _openGraph_siteName :: Text,
        _openGraph_type :: Text, -- TODO: ADT (different types with their own properties)
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
    whenJust _openGraph_image $ \uri -> do
      if isJust (URI.uriScheme uri)
        then metaOg "image" $ URI.render uri
        else error $ "OGP image URL must be absolute. This URI is not: " <> URI.render uri
    where
      -- Open graph meta element
      metaOg k v =
        meta_
          [ makeAttribute "property" $ "og:" <> k,
            content_ v
          ]
      meta' k v = meta_ [name_ k, content_ v]
