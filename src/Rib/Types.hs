{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Rib.Types
  ( Page(..)
  , Post(..)
  , PostFilePath(..)
  ) where

import Control.Monad
import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics (Generic)

import Text.Pandoc (Pandoc)
import Development.Shake.Classes (Binary, Hashable, NFData)

-- | Represents a HTML page that will be generated
data Page
  = Page_Index [Post]
  | Page_Post Post
  deriving (Generic, Show, FromJSON, ToJSON)

-- | A JSON serializable representation of a post's metadata
data Post = Post
  { _post_doc :: Pandoc
  , _post_url :: Text
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON)

-- A simple wrapper data-type which implements 'ShakeValue';
-- Used as a Shake Cache key to build a cache of post objects.
newtype PostFilePath = PostFilePath FilePath
  deriving (Show, Eq, Hashable, Binary, NFData, Generic)
