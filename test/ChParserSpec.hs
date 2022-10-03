{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
module ChParserSpec (spec) where

import Test.Hspec
import Text.Trifecta
import LearnParsers

spec :: Spec
spec = do
  describe "Parser SemVer" $ do
    let
      ps = parseString
      psv = ps parseSemVer mempty
    it "2.1.1" $ do
      let
        (Success psvr) = psv "2.1.1"
      psvr `shouldBe` (SemVer 2 1 1 [] [])
    it "1.0.0-x.7.z.92" $ do
      let
        (Success psvr) = psv "1.0.0-x.7.z.92"
      psvr `shouldBe` (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])