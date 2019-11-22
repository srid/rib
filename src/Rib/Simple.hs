{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Sensible defaults for writing the most simple static site
module Rib.Simple where

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Development.Shake (Action)
import Lucid (Html)

-- import Rib.Reader.Pandoc (getMeta)
import Rib.Shake
import Rib.Reader

-- | Type of page to be generated
data Page doc
  = Page_Index [(FilePath, doc)]
  -- ^ Index page linking to a list of posts
  | Page_Post (FilePath, doc)
  -- ^ Individual post page
  deriving (Generic, Show, FromJSON, ToJSON)

-- | Shake build action for the most simple static site
--
-- - Copies @static/@ as is.
-- - Builds @*.md@, @*.rst@ and @*.org@ as HTML
-- - Builds an @index.html@ of all pages unless `draft` metadata is set to `True`.
buildAction
  :: forall doc.
     (RibReader doc)
  => (Page doc -> Html ())
  -> Action ()
buildAction renderPage = do
  void $ buildStaticFiles ["static/**"]
  posts <- buildHtmlMulti "*.md" $ renderPage . Page_Post
  buildHtml "index.html" $
    renderPage $ Page_Index posts
