{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Rib.CliSpec
  ( spec,
  )
where

import Relude
import Rib.Cli
import Test.Hspec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "Host and Port parsing" $ do
    it "should parse port" $ do
      parseHostPort ":8080" `shouldBe` Right ("127.0.0.1", 8080)
    it "should parse localhost" $ do
      parseHostPort "localhost:8080" `shouldBe` Right ("localhost", 8080)
    it "should parse IP addr" $ do
      parseHostPort "132.45.0.254:8080" `shouldBe` Right ("132.45.0.254", 8080)
  where
    parseHostPort =
      parse hostPortParser "<test>"
