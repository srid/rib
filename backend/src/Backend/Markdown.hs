{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Markdown where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml

import Text.Megaparsec.Error (errorBundlePretty)
import qualified Text.MMark as MMark

-- This code is unused now

data Page = Page
  { _page_title :: Text
  , _page_content :: MMark.MMark
  }
  deriving Show

parsePage :: MonadIO m => Text -> m (Either Text Page)
parsePage articleName = do
  let articlePath = T.unpack $ "articles/" <> articleName <> ".md"
  content <- liftIO $ T.readFile articlePath
  case MMark.parse (T.unpack articleName) content of
    Left err -> pure $ Left $ T.pack $ errorBundlePretty err
    Right v -> case MMark.projectYaml v of
      Nothing -> pure $ Left "No Yaml found"
      Just doc -> do
        let f = Yaml.withObject "metadata" $ \metadata -> do
                title <- metadata .: "title"
                pure $ Page title v
        pure $ first T.pack $ Yaml.parseEither f doc
