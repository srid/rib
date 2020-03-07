{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Type-safe routes
module Rib.Route
  ( IsRoute (..),
    routeUrl,
    routeUrlRel,
    writeRoute,
  )
where

import Control.Monad.Catch
import Data.Kind
import qualified Data.Text as T
import Development.Shake (Action)
import Path
import Relude
import Rib.Shake (writeFileCached)

-- | A route is a GADT representing individual routes.
--
-- The GADT type parameter represents the data used to render that particular route.
class IsRoute (r :: Type -> Type) where
  -- | Return the filepath (relative `Rib.Shake.ribInputDir`) where the
  -- generated content for this route should be written.
  routeFile :: MonadThrow m => r a -> m (Path Rel File)

data UrlType = Relative | Absolute

path2Url :: Path Rel File -> UrlType -> Text
path2Url fp = toText . toFilePath . \case
  Relative ->
    fp
  Absolute ->
    [absdir|/|] </> fp

-- | The absolute URL to this route (relative to site root)
routeUrl :: IsRoute r => r a -> Text
routeUrl = routeUrl' Absolute

-- | The relative URL to this route
routeUrlRel :: IsRoute r => r a -> Text
routeUrlRel = routeUrl' Relative

-- | Get the URL to a route
routeUrl' :: IsRoute r => UrlType -> r a -> Text
routeUrl' urlType = stripIndexHtml . flip path2Url urlType . either (error . toText . displayException) id . routeFile
  where
    stripIndexHtml s =
      -- Because path2Url can return relative URL, we must account for there
      -- not being a / at the beginning.
      if  | "/index.html" `T.isSuffixOf` s ->
            T.dropEnd (T.length "index.html") s
          | s == "index.html" ->
            "."
          | otherwise ->
            s

-- | Write the content `s` to the file corresponding to the given route.
writeRoute :: (IsRoute r, ToString s) => r a -> s -> Action ()
writeRoute r content = do
  fp <- liftIO $ routeFile r
  writeFileCached fp $ toString $ content
