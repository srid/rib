{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Default (def)

import qualified Rib.Settings as S
import qualified Rib.App as App

import qualified HTML


-- | Configure this site here.
--
-- See `S.Settings` for the settings available.
siteSettings :: S.Settings
siteSettings = def
  { S.pageHTML = HTML.pageHTML
  -- ^ How to render a page type
  }

main :: IO ()
main = App.run siteSettings

-- | Entrypoint suited for ghcid
--
-- See README.md for usage
dev :: IO ()
dev = App.runWith siteSettings $ App.Serve 8080 True
