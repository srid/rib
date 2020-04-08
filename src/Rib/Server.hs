{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Serve generated static files with HTTP
module Rib.Server
  ( serve,
  )
where

import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import Relude
import Rib.Settings

-- | Run a HTTP server to serve a directory of static files
--
-- Binds the server to host 127.0.0.1.
serve ::
  RibSettings ->
  -- | Port number to bind to
  Int ->
  -- | Directory to serve.
  FilePath ->
  IO ()
serve ribSettings port path = do
  logStrLn ribSettings $ "[Rib] Serving " <> path <> " at http://" <> host <> ":" <> show port
  Warp.runSettings settings app
  where
    app = staticApp $ defaultFileServerSettings path
    host = "127.0.0.1"
    settings =
      Warp.setHost (fromString host)
        $ Warp.setPort port
        $ Warp.defaultSettings
