{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Pandoc.Util where

import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Text as T

import Reflex.Dom.Core

elPandocAttr
  :: DomBuilder t m
  => Text
  -- ^ Element name
  -> (String, [String], [(String, String)])
  -- ^ Pandoc attribute object. TODO: Use a sensible type.
  -> m a
  -- ^ Child widget
  -> m a
elPandocAttr name = elAttr name . renderAttr

renderAttr
  :: (String, [String], [(String, String)])
  -- ^ Pandoc attribute object. TODO: Use a sensible type.
  -> Map Text Text
renderAttr (identifier, classes, attrs) =
     "id" =: T.pack identifier
  <> "class" =: T.pack (unwords classes)
  <> Map.fromList ((\(x,y) -> (T.pack x, T.pack y)) <$> attrs)

addClass
  :: String
  -- ^ The class to add
  -> (String, [String], [(String, String)])
  -- ^ Pandoc attribute object. TODO: Use a sensible type.
  -> (String, [String], [(String, String)])
addClass c (identifier, classes, attrs) = (identifier, c : classes, attrs)

headerElement :: Int -> Text
headerElement level = case level of
  1 -> "h1"
  2 -> "h2"
  3 -> "h3"
  4 -> "h4"
  5 -> "h5"
  6 -> "h6"
  _ -> error "bad header level"
