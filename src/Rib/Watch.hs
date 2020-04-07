{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Filesystem watching using fsnotify
module Rib.Watch
  ( onTreeChange,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan
import Path
import Relude
import System.FSNotify (Event (..), watchTreeChan, withManager)

-- | Recursively monitor the contents of the given path and invoke the given IO
-- action for every event triggered.
--
-- If multiple events fire rapidly, the IO action is invoked only once, taking
-- those multiple events as its argument.
onTreeChange :: Path b t -> ([Event] -> IO ()) -> IO ()
onTreeChange fp f = do
  withManager $ \mgr -> do
    eventCh <- newChan
    void $ watchTreeChan mgr (toFilePath fp) (const True) eventCh
    forever $ do
      firstEvent <- readChan eventCh
      events <- debounce 100 [firstEvent] $ readChan eventCh
      f events

debounce :: Int -> [event] -> IO event -> IO [event]
debounce millies events f = do
  -- Race the readEvent against the timelimit.
  race f (threadDelay (1000 * millies)) >>= \case
    Left event ->
      -- If the read event finishes first try again.
      debounce millies (events <> [event]) f
    Right () ->
      -- Otherwise continue
      return events
