{-# LANGUAGE OverloadedStrings #-}

-- | Serve generated static files with HTTP
module Rib.Server
  ( serve,
  )
where

import Network.Wai.Application.Static (defaultFileServerSettings, ssListing, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import WaiAppStatic.Types (StaticSettings)

-- | WAI Settings suited for serving statically generated websites.
staticSiteServerSettings :: FilePath -> StaticSettings
staticSiteServerSettings root =
  defaultSettings
    { ssListing = Nothing -- Disable directory listings
    }
  where
    defaultSettings = defaultFileServerSettings root

-- | Run a HTTP server to serve a directory of static files
serve ::
  -- | Port number to bind to
  Int ->
  -- | Directory to serve.
  FilePath ->
  IO ()
serve port path = do
  putStrLn $ "[Rib] Serving at http://localhost:" <> show port
  Warp.run port $ staticApp $ staticSiteServerSettings path
