{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Rib.Server where

import Prelude hiding (init, last)

import Control.Monad (guard)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Safe (initMay, lastMay)

import Development.Shake.FilePath
import Network.Wai.Application.Static (defaultFileServerSettings, ssListing, ssLookupFile, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import WaiAppStatic.Types (LookupResult (..), Pieces, StaticSettings, fromPiece, unsafeToPiece)

-- | WAI Settings suited for serving statically generated websites.
staticSiteServerSettings :: FilePath -> StaticSettings
staticSiteServerSettings root = settings
  { ssLookupFile = lookupFileForgivingHtmlExt
  , ssListing = Nothing  -- Disable directory listings
  }
  where
    settings = defaultFileServerSettings root

    -- | Like upstream's `ssLookupFile` but ignores the ".html" suffix in the
    -- URL when looking up the corresponding file in the filesystem.
    --
    -- This allows "clean urls" so to speak.
    lookupFileForgivingHtmlExt :: Pieces -> IO LookupResult
    lookupFileForgivingHtmlExt pieces = ssLookupFile settings pieces >>= \case
      LRNotFound -> ssLookupFile settings (addHtmlExt pieces)
      x -> pure x

    -- | Add the ".html" suffix to the URL unless it already exists
    addHtmlExt :: Pieces -> Pieces
    addHtmlExt xs = fromMaybe xs $ do
      init <- fmap fromPiece <$> initMay xs
      last <- fromPiece <$> lastMay xs
      guard $ not $ ".html" `isSuffixOf` T.unpack last
      pure $ fmap unsafeToPiece $ init <> [last <> ".html"]

-- | Return the URL for the given .html file under serve directory
--
-- File path must be relative to the serve directory.
--
-- You may also pass source paths as long as they map directly to destination
-- path except for file extension.
getHTMLFileUrl :: FilePath -> Text
getHTMLFileUrl = T.pack . ("/" ++) . dropExtension

-- | Run a HTTP server to serve a directory of static files
--
-- Allow URLs of the form `/foo/bar` to serve ${path}/foo/bar.html
serve
  :: Int
  -- ^ Port number to bind to
  -> FilePath
  -- ^ Directory to serve.
  -> IO ()
serve port path = do
  putStrLn $ "[Rib] Serving at http://localhost:" <> show port
  Warp.run port $ staticApp $ staticSiteServerSettings path
