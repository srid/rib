{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend where

import Data.Functor.Identity
import Data.Functor.Sum
import Data.Text (Text)

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Common.Route
import Common.Markdown

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Explaining Haskell"
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: semuiCdnUrl "semantic.min.css") blank

  , _frontend_body = divClass "ui container" $ do
      subRoute_ $ \case
        FrontendRoute_Main -> do
          elClass "h1" "ui header" $ text "Explaining Haskell"

          elClass "h2" "ui header" $ do
            text "Purpose of this site"
            divClass "sub header" $ text "Else why do it?"
          el "p" $ text "The purpose of this site is twofold:"
          el "ol" $ do
            el "li" $ text "Record everything I learn in regards to Haskell for future reference"
            el "li" $ text "Attempt to teach what I understand to others in the hopes of strengthening my own understanding"

          elClass "h2" "ui header" $ do
            text "Game plan"
            divClass "sub header" $ text "How I plan to finish this"
          el "p" $ text "See README.md"

          elClass "h2" "ui header" $ text "List of articles"
          el "ul" $
            el "li" $ routeLink (FrontendRoute_Article :/ "NixTutorial") $ text "Nix Tutorial"
        FrontendRoute_Article -> do
          articleContent <- getArticle =<< askRoute
          prerender_ (text "JavaScript is required to view this page") blank
          widgetHold_ (el "div" $ text "Loading") $ ffor articleContent $ \case
            Nothing -> text "nope"
            Just s -> divClass "ui segment" $ elMarkdown s
  }
  where
    semuiVersion :: Text
    semuiVersion = "2.4.2"
    semuiCdnUrl :: Text -> Text
    semuiCdnUrl file = "https://cdn.jsdelivr.net/npm/semantic-ui@" <> semuiVersion <> "/dist/" <> file


getArticle
  :: (MonadHold t m, PostBuild t m, Prerender js t m)
  => Dynamic t Text -> m (Event t (Maybe Text))
getArticle articleName =
  fmap switchDyn $ prerender (pure never) $ do
    pb <- getPostBuild
    getAndDecode $ ffor (tag (current articleName) pb) $ \an ->
      renderBackendRoute enc (BackendRoute_GetArticle :/ an)
  where
    Right (enc :: Encoder Identity Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName) = checkEncoder backendRouteEncoder
