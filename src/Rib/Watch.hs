{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Filesystem watch
module Rib.Watch
  ( onTreeChange,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan
import Control.Monad.Catch
import Path
import Path.IO
import Relude
import System.FSNotify (Event (..), eventIsDirectory, eventPath, watchTreeChan, withManager)

-- | Recursively monitor the contents of `fp` and invoke `f` for every event.
--
-- If multiple events fire rapidly, accumulate them in a list and call `f` once.
--
-- `workDir` denotes the current working directory.
onTreeChange :: Path b t -> ([Event] -> IO ()) -> IO ()
onTreeChange fp f = do
  workDir <- getCurrentDir
  withManager $ \mgr -> do
    eventCh <- newChan
    void $ watchTreeChan mgr (toFilePath fp) (const True) eventCh
    forever $ do
      firstEvent <- readEvent workDir eventCh
      events <- debounce 100 [firstEvent] $ readEvent workDir eventCh
      f events

readEvent :: Path Abs Dir -> Chan Event -> IO Event
readEvent workDir events = do
  e <- readChan events
  logEvent workDir e
  pure e

debounce :: Int -> [event] -> IO event -> IO [event]
debounce millies events f = do
  -- race the readEvent against the timelimit.
  race f (threadDelay (1000 * millies)) >>= \case
    Left event ->
      -- if the read event finishes first try again.
      debounce millies (events <> [event]) f
    Right () ->
      -- otherwise continue
      return events

logEvent :: (MonadThrow m, MonadIO m) => Path Abs Dir -> Event -> m ()
logEvent workDir e = do
  eventRelPath <-
    if eventIsDirectory e
      then fmap toFilePath . makeRelative workDir =<< parseAbsDir (eventPath e)
      else fmap toFilePath . makeRelative workDir =<< parseAbsFile (eventPath e)
  putStrLn $ eventLogPrefix e <> " " <> eventRelPath

eventLogPrefix :: Event -> String
eventLogPrefix = \case
  -- Single character log prefix to indicate file actions is a convention in Rib.
  Added _ _ _ -> "A"
  Modified _ _ _ -> "M"
  Removed _ _ _ -> "D"
  Unknown _ _ _ -> "?"
