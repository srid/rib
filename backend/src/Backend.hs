{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import qualified Data.Aeson as Aeson
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Obelisk.Backend
import Obelisk.Route
import Snap
import Reflex.Dom.Core

import Common.Route
import Backend.Markdown

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
      BackendRoute_Missing :/ () ->
        pure ()
      BackendRoute_GetArticle :/ articleName -> do
        let articlePath = "articles/" <> articleName <> ".md"
        c <- liftIO (T.readFile $ T.unpack articlePath)
        (_, bs) <- liftIO $ renderStatic $ elMarkdown c
        writeLBS $ Aeson.encode $ T.decodeUtf8  bs
  , _backend_routeEncoder = backendRouteEncoder
  }
