{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
module ChMonoid.OptionalSpec (spec) where

import Test.Hspec
import Data.Monoid
import Test.Hspec.QuickCheck (prop)
import ChMonoid.Optional
import Test.QuickCheck (quickCheck)

semigroupAssoc :: (Semigroup m, Show m, Eq m) => m -> m -> m -> Expectation
semigroupAssoc a b c = (a <> b) <> c `shouldBe` a <> (b <> c)

semigroupAssocFun :: (Semigroup b, Show b, Eq b) => b -> b -> b -> a -> Expectation
semigroupAssocFun x y z d = (unCombine $ (f <> g) <> h) d `shouldBe` (unCombine $ f <> (g <> h)) d
  where
    f = Combine (const x)
    g = Combine (const y)
    h = Combine (const z)

monoidLeftIdentity :: (Eq m, Show m, Monoid m) => m -> Expectation
monoidLeftIdentity a = (mempty <> a) `shouldBe` a

monoidRightIdentity :: (Eq m, Show m, Monoid m) => m -> Expectation
monoidRightIdentity a = (a <> mempty) `shouldBe` a

spec :: Spec
spec = do
  describe "Testing Only with Monoids" $ do
    let
      onlySum = Only (Sum 1)
      onlyFour = Only (Product 4)
      onlyTwo = Only (Product 2)
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

  describe "Testing Semigroup First'" $ do
    let
      onlyOne = First' (Only 1)
      onlyTwo = First' (Only 2)
      nada = First' Nada :: First' Integer
    it "onlyOne <> Nada" $ do
      onlyOne `mappend` nada `shouldBe` First' {getFirst' = Only 1}
    it "Nada <> Nada" $ do
      nada `mappend` nada `shouldBe` First' {getFirst' = Nada}
    it "Nada <> onlyTwo" $ do
      nada `mappend` onlyTwo `shouldBe` First' {getFirst' = Only 2}
    it "onlyOne <> onlyTwo" $ do
      onlyOne `mappend` onlyTwo `shouldBe` First' {getFirst' = Only 1}

  describe "Testing Semigroup Or" $ do
    it "Fst 1 <> Snd 2" $ do
      Fst 1 <> Snd 2 `shouldBe` Snd 2
    it "Fst 1 <> Fst 2" $ do
      Fst 1 <> Fst 2 `shouldBe` (Fst 2 :: Or Integer Integer)
    it "Snd 1 <> Fst 2" $ do
      Snd 1 <> Fst 2 `shouldBe` Snd 1
    it "Snd 1 <> Snd 2" $ do
      Snd 1 <> Snd 2 `shouldBe` (Snd 1 :: Or Integer Integer)

  describe "Testing Semigroup Combine" $ do
    let
      f = Combine $ \n -> Sum (n + 1)
      g = Combine $ \n -> Sum (n - 1)
    it "(f <> g) 0" $ do
      unCombine (f <> g) 0 `shouldBe` Sum {getSum = 0}
    it "(f <> g) 1" $ do
      unCombine (f <> g) 1 `shouldBe` Sum {getSum = 2}
    it "(f <> f) 1" $ do
      unCombine (f <> f) 1 `shouldBe` Sum {getSum = 4}
    it "(g <> f) 1" $ do
      unCombine (g <> f) 1 `shouldBe` Sum {getSum = 2}

  context "Monoid String" $ do
    prop "associative" (semigroupAssoc :: String -> String -> String -> Expectation)
    prop "Left Identity" (monoidLeftIdentity :: String -> Expectation)
    prop "Right Identity" (monoidRightIdentity :: String -> Expectation)

  context "Monoid Bull" $ do
    prop "associative" (semigroupAssoc :: Bull -> Bull -> Bull -> Expectation)
    prop "Left Identity" (monoidLeftIdentity :: Bull -> Expectation)
    prop "Right Identity" (monoidRightIdentity :: Bull -> Expectation)

  context "Monoid First'" $ do
    prop "associative" (semigroupAssoc :: First' Integer -> First' Integer -> First' Integer-> Expectation)
    prop "Left Identity" (monoidLeftIdentity :: First' Integer -> Expectation)
    prop "Right Identity" (monoidRightIdentity :: First' Integer -> Expectation)

  describe "Type Trivial" $ do
    context "Semigroup properties" $ do
      prop "associative" (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Expectation)

  describe "Type Identity" $ do
    context "Semigroup properties" $ do
      prop "associative" (semigroupAssoc :: Identity Integer -> Identity Integer -> Identity Integer -> Expectation)

  describe "Type BoolConj" $ do
    context "Semigroup properties" $ do
      prop "associative" (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Expectation)

  describe "Type BoolDisj" $ do
    context "Semigroup properties" $ do
      prop "associative" (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Expectation)

  describe "Type Or" $ do
    context "Semigroup properties" $ do
      prop "associative" (semigroupAssoc :: Or Integer Integer -> Or Integer Integer -> Or Integer Integer -> Expectation)

  describe "Type Combine" $ do
    it "Semigroup properties" $ do
      quickCheck (semigroupAssocFun :: String -> String -> String -> Integer -> Expectation)