{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Serve generated static files with HTTP
module Rib.Server
  ( serve,
  )
where

import Network.Wai.Application.Static (defaultFileServerSettings, ssListing, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import Path
import Relude
import Rib.Logging
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
--
-- Binds the server to host 127.0.0.1.
serve ::
  Typeable b =>
  RibSettings ->
  -- | Port number to bind to
  Int ->
  -- | Directory to serve.
  Path b Dir ->
  IO ()
serve ribSettings port path = do
  pathS <- prettyPrintPathDir ribSettings path
  putStrLn $ "[Rib] Serving " <> toString pathS <> " at http://" <> host <> ":" <> show port
  Warp.runSettings settings app
  where
    app = staticApp $ staticSiteServerSettings $ toFilePath path
    host = "127.0.0.1"
    settings =
      Warp.setHost (fromString host)
        $ Warp.setPort port
        $ Warp.defaultSettings
