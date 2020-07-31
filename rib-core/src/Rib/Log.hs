{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Rib.Log
  ( logStrLn,
    logErr,
  )
where

import Development.Shake (Verbosity (..))
import Relude
import Rib.Cli (CliConfig (..))
import System.IO (hPutStrLn)

logStrLn :: MonadIO m => CliConfig -> String -> m ()
logStrLn CliConfig {..} s =
  unless (verbosity == Silent) $ do
    putStrLn s

logErr :: MonadIO m => String -> m ()
logErr s =
  liftIO $ hPutStrLn stderr s
