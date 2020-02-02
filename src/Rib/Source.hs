{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Source where

import Development.Shake (Action)
import Path
import Relude

-- | A function that parses a source representation out of the given file
type SourceReader repr = forall b. Path b File -> Action (Either Text repr)
