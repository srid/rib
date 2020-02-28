{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Route
  ( IsRoute (..),
    routeUrl,
    writeRoute,
  )
where

import Control.Monad.Catch
import Data.Kind
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake (Action, liftIO)
import Path
import Relude
import Rib.Shake (writeFileCached)

-- | A route is a GADT representing individual routes.
--
-- The GADT type parameter represents the data used to render that particular route.
class IsRoute (r :: Type -> Type) where
  routeFile :: MonadThrow m => r a -> m (Path Rel File)

-- | Get the URL to a route
routeUrl :: IsRoute r => r a -> Text
routeUrl = urlForPath . either (error . toText . displayException) id . routeFile
  where
    urlForPath :: Path Rel File -> Text
    urlForPath = stripIndexHtml . relPathToUrl
      where
        relPathToUrl = toText . toFilePath . ([absdir|/|] </>)
        stripIndexHtml s =
          if T.isSuffixOf "index.html" s
            then T.dropEnd (T.length $ "index.html") s
            else s

writeRoute :: (IsRoute r, ToString s) => r a -> s -> Action ()
writeRoute r content = do
  fp <- liftIO $ routeFile r
  writeFileCached fp $ toString $ content
