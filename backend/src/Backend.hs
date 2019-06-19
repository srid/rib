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

import Obelisk.Backend
import Obelisk.Route
import Snap

import Common.Route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
      BackendRoute_Missing :/ () ->
        pure ()
      BackendRoute_GetArticle :/ articleName -> do
        let articlePath = "articles/" <> articleName <> ".md"
        c <- liftIO (T.readFile $ T.unpack articlePath)
        writeLBS $ Aeson.encode c
  , _backend_routeEncoder = backendRouteEncoder
  }
