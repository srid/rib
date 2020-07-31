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
import Rib.Cli (CliConfig)
import Rib.Log

-- | Run a HTTP server to serve a directory of static files
--
-- Binds the server to host 127.0.0.1.
serve ::
  CliConfig ->
  -- | Host
  Text ->
  -- | Port number to bind to
  Int ->
  -- | Directory to serve.
  FilePath ->
  IO ()
serve cfg host port path = do
  logStrLn cfg $ "[Rib] Serving " <> path <> " at http://" <> toString host <> ":" <> show port
  Warp.runSettings settings app
  where
    app = staticApp $ defaultFileServerSettings path
    settings =
      Warp.setHost (fromString $ toString host)
        $ Warp.setPort port
        $ Warp.defaultSettings
