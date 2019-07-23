{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- Sensible defaults for writing the most simple static site
module Rib.Simple where

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

import Development.Shake (Action)
import Lucid (Html)
import Text.Pandoc (Pandoc, readMarkdown, readRST, readOrg)

import Rib.Pandoc (getMeta)
import Rib.Shake

-- | An HTML page that will be generated
data Page
  = Page_Index [(FilePath, Pandoc)]
  | Page_Post (FilePath, Pandoc)
  deriving (Generic, Show, FromJSON, ToJSON)

buildAction :: (Page -> Html ()) -> Action ()
buildAction renderPage = do
  void $ buildStaticFiles ["static/**"]
  posts <- concat <$> forM pats
    (flip buildHtmlMulti $ renderPage . Page_Post)
  let publicPosts = filter (not . isDraft . snd) posts
  buildHtml "index.html" $
    renderPage $ Page_Index publicPosts
  where
    isDraft = fromMaybe False . getMeta @Bool "draft"
    pats =
      [ ("*.md", readMarkdown)
      , ("*.rst", readRST)
      , ("*.org", readOrg)
      ]
