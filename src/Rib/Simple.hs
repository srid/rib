{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Sensible defaults for writing the most simple static site
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

-- | Type of page to be generated
data Page
  = Page_Index [(FilePath, Pandoc)]
  -- ^ Index page linking to a list of posts
  | Page_Post (FilePath, Pandoc)
  -- ^ Individual post page
  deriving (Generic, Show, FromJSON, ToJSON)

-- | Shake build action for the most simple static site
--
-- - Copies @static/@ as is.
-- - Builds @*.md@, @*.rst@ and @*.org@ as HTML
-- - Builds an @index.html@ of all pages unless `draft` metadata is set to `True`.
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
