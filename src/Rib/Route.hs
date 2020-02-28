{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Route
  ( R,
    pattern (:/),
    IsRoute (..),
    routeUrl,
    writeRoute,
    Some (..),
  )
where

import Control.Monad.Catch
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Kind
import Data.Some
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Development.Shake (Action, liftIO)
import Path
import Relude
import Rib.Shake (urlForPath, writeFileCached)

type R f = DSum f Identity

-- | Convenience builder for a 'R' using 'Identity' for the functor.
pattern (:/) :: f a -> a -> R f
pattern a :/ b = a :=> Identity b

{-# COMPLETE (:/) #-}

infixr 5 :/

-- | A route is a GADT representing individual routes.
--
-- The GADT type parameter represents the data used to render that particular route.
class IsRoute (r :: Type -> Type) where
  routeFile :: MonadThrow m => Some r -> m (Path Rel File)

-- | Get the URL to a route
routeUrl :: IsRoute r => Some r -> Text
routeUrl = urlForPath . either (error . toText . displayException) id . routeFile

writeRoute :: IsRoute r => r a -> TL.Text -> Action ()
writeRoute r content = do
  fp <- liftIO $ routeFile $ Some r
  writeFileCached fp $ T.unpack $ TL.toStrict $ content
