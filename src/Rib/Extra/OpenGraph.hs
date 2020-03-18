{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Meta tags for The Open Graph protocol: https://ogp.me/
module Rib.Extra.OpenGraph
  ( OpenGraph (..),
    OGType (..),
    Article (..),
  )
where

import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import Lucid
import Lucid.Base (makeAttribute)
import Relude
import qualified Text.URI as URI

-- The OpenGraph metadata
--
-- This type can be directly rendered to HTML using `toHTML`.
data OpenGraph
  = OpenGraph
      { _openGraph_title :: Text,
        _openGraph_url :: Maybe URI.URI,
        _openGraph_author :: Maybe Text,
        _openGraph_description :: Maybe Text,
        _openGraph_siteName :: Text,
        _openGraph_type :: Maybe OGType,
        _openGraph_image :: Maybe URI.URI
      }
  deriving (Eq, Show)

instance ToHtml OpenGraph where
  toHtmlRaw = toHtml
  toHtml OpenGraph {..} = do
    meta' "author" `mapM_` _openGraph_author
    meta' "description" `mapM_` _openGraph_description
    requireAbsolute "OGP URL" (\uri -> link_ [rel_ "canonical", href_ uri]) `mapM_` _openGraph_url
    metaOg "title" _openGraph_title
    metaOg "site_name" _openGraph_siteName
    toHtml `mapM_` _openGraph_type
    requireAbsolute "OGP image URL" (metaOg "image") `mapM_` _openGraph_image
    where
      meta' k v = meta_ [name_ k, content_ v]
      requireAbsolute description f uri =
        if isJust (URI.uriScheme uri)
          then f $ URI.render uri
          else error $ description <> " must be absolute. this URI is not: " <> URI.render uri

-- TODO: Remaining ADT values & sub-fields
data OGType
  = OGType_Article Article
  | OGType_Website
  deriving (Eq, Show)

instance ToHtml OGType where
  toHtmlRaw = toHtml
  toHtml = \case
    OGType_Article article -> do
      metaOg "type" "article"
      toHtml article
    OGType_Website -> do
      metaOg "type" "website"

-- TODO: _article_profile :: [Profile]
data Article
  = Article
      { _article_section :: Maybe Text,
        _article_modifiedTime :: Maybe UTCTime,
        _article_publishedTime :: Maybe UTCTime,
        _article_expirationTime :: Maybe UTCTime,
        _article_tag :: [Text]
      }
  deriving (Eq, Show)

instance ToHtml Article where
  toHtmlRaw = toHtml
  toHtml Article {..} = do
    metaOg "article:section" `mapM_` _article_section
    metaOgTime "article:modified_time" `mapM_` _article_modifiedTime
    metaOgTime "article:published_time" `mapM_` _article_publishedTime
    metaOgTime "article:expiration_time" `mapM_` _article_expirationTime
    metaOg "article:tag" `mapM_` _article_tag
    where
      metaOgTime k t =
        metaOg k $ toText $ formatShow iso8601Format t

-- Open graph meta element
metaOg :: Applicative m => Text -> Text -> HtmlT m ()
metaOg k v =
  meta_
    [ makeAttribute "property" $ "og:" <> k,
      content_ v
    ]
