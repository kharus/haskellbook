{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
module ChMonoid.OptionalSpec (spec) where

import Test.Hspec
import Data.Monoid
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import ChMonoid.Optional

monoidAssoc :: (Monoid m, Show m, Eq m) => m -> m -> m -> Expectation
monoidAssoc a b c = (a <> b) <> c `shouldBe` a <> (b <> c)

monoidLeftIdentity :: (Eq m, Show m, Monoid m) => m -> Expectation
monoidLeftIdentity a = (mempty <> a) `shouldBe` a

monoidRightIdentity :: (Eq m, Show m, Monoid m) => m -> Expectation
monoidRightIdentity a = (a <> mempty) `shouldBe` a

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools) , (1, return Twoo) ]

spec :: Spec
spec = do
  let onlySum = Only (Sum 1)
      onlyFour = Only (Product 4)
      onlyTwo = Only (Product 2)
  describe "Testing Only with Monoids" $ do
    it "Sum" $ do
      onlySum `mappend` onlySum `shouldBe` Only (Sum {getSum = 2})
    it "Product" $ do
      onlyFour `mappend` onlyTwo `shouldBe` Only (Product {getProduct = 8})
    it "Only a <> Nada" $ do
      Only (Sum 1) `mappend` Nada `shouldBe` Only (Sum {getSum = 1})
    it "Only b <> Nada" $ do
      Only [1] `mappend` Nada `shouldBe` Only [1]
    it "Nada <> Only a" $ do
      Nada `mappend` Only (Sum 1) `shouldBe` Only (Sum {getSum = 1})

  context "Monoid String" $ do
    prop "associative" (monoidAssoc :: String -> String -> String -> Expectation)
    prop "Left Identity" (monoidLeftIdentity :: String -> Expectation)
    prop "Right Identity" (monoidRightIdentity :: String -> Expectation)

  context "Monoid Bull" $ do
    prop "associative" (monoidAssoc :: Bull -> Bull -> Bull -> Expectation)
    prop "Left Identity" (monoidLeftIdentity :: Bull -> Expectation)
    prop "Right Identity" (monoidRightIdentity :: Bull -> Expectation)