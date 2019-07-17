{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Sensible defaults for writing the most simple static site
module Rib.Simple where

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe
import GHC.Generics (Generic)

import Development.Shake
import Lucid
import Text.Pandoc (Pandoc)

import Rib.Pandoc (getPandocMetaValue)
import Rib.Shake

-- | An HTML page that will be generated
data Page
  = Page_Index [(FilePath, Pandoc)]
  | Page_Post (FilePath, Pandoc)
  deriving (Generic, Show, FromJSON, ToJSON)

-- TODO: Eventually this should be subsumed into our Pandoc metadata system.
isDraft :: Pandoc -> Bool
isDraft = fromMaybe False . getPandocMetaValue "draft"

buildAction :: (Page -> Html ()) -> Action ()
buildAction renderPage = do
  void $ buildStaticFiles ["static/**"]
  posts <- buildHtmlMulti ["*.md"] $ renderPage . Page_Post
  let publicPosts = filter (not . isDraft . snd) posts
  buildHtml "index.html" $  renderPage $ Page_Index publicPosts

-- TODO: Get get rid of the above, and have Main.hs do it manually.
