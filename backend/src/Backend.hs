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
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Data.Profunctor

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
            -- | Like T.readFile but takes path as `Text` and outputs content as JSON encoded text.
            readFile' = dimap T.unpack (fmap Aeson.encode) T.readFile
        content <- liftIO $ readFile' articlePath
        writeLBS content
  , _backend_routeEncoder = backendRouteEncoder
  }
