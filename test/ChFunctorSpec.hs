{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
module ChFunctorSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (quickCheck)
import ChFunctor
import Types.Identity
import Types.BahEither
import Types.List
import Types.Constant
import Types.Two
import Test.QuickCheck.Classes
import Test.Hspec.Checkers (testBatch)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>(a -> b)-> (b -> c)-> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

spec :: Spec
spec = do
  describe "Testing Only with Monoids" $ do
    it "Sum" $ do
      (fmap (+1) $ read "[1]" :: [Int]) `shouldBe` [2]
    it "Sum" $ do
      fmap (fmap (++ "lol")) (Just ["Hi,", "Hello"]) `shouldBe` Just ["Hi,lol","Hellolol"]

  describe "Identity" $ do
    testBatch (semigroup (undefined :: Identity String, 1::Int))
    testBatch (monoid (undefined :: Identity String))
    testBatch (functor (undefined :: Identity (String, String, Int)))
    testBatch (applicative (undefined :: Identity (String, String, Int)))
    testBatch (monad (undefined :: Identity (String, String, Int)))

  context "Functor Pair" $ do
    prop "Identity" (functorIdentity :: Pair Int -> Bool)
    prop "Compose" (functorCompose (+1) (*2) :: Pair Int -> Bool)

  context "Functor Two" $ do
    prop "Identity" (functorIdentity :: Two Int Int -> Bool)
    prop "Compose" (functorCompose (+1) (*2) ::  Two Int Int -> Bool)

  context "Functor Three" $ do
    prop "Identity" (functorIdentity :: Three Int Int Int -> Bool)
    prop "Compose" (functorCompose (+1) (*2) ::  Three Int Int Int-> Bool)

  context "Functor Three'" $ do
    prop "Identity" (functorIdentity :: Three' Int Int -> Bool)
    prop "Compose" (functorCompose (+1) (*2) ::  Three' Int Int-> Bool)

  context "Functor Four" $ do
    prop "Identity" (functorIdentity :: Four Int Int Int Int -> Bool)
    prop "Compose" (functorCompose (+1) (*2) ::  Four Int Int Int Int -> Bool)

  context "Functor Four'" $ do
    prop "Identity" (functorIdentity :: Four' Int Int -> Bool)
    prop "Compose" (functorCompose (+1) (*2) ::  Four' Int Int -> Bool)

  context "Functor Possibly'" $ do
    prop "Identity" (functorIdentity :: Possibly Int -> Bool)
    prop "Compose" (functorCompose (+1) (*2) ::  Possibly Int -> Bool)

  describe "MyList" $ do
    testBatch (monoid Twoo)

  describe "MyList" $ do
    testBatch (functor (undefined :: List (String, String, Int)))
    testBatch (applicative (undefined :: List (String, String, Int)))
    testBatch (monad (undefined :: List (String, String, Int)))

  describe "Sum" $ do
    testBatch (functor (undefined :: Sum String (String, String, Int)))
    testBatch (applicative (undefined :: Sum String (String, String, Int)))
    testBatch (monad (undefined :: Sum String (String, String, Int)))

  describe "Nope" $ do
    testBatch (functor (undefined :: Nope  (String, String, Int)))
    testBatch (applicative (undefined :: Nope  (String, String, Int)))
    testBatch (monad (undefined :: Nope (String, String, Int)))

  describe "BahEither" $ do
    testBatch (functor (undefined :: BahEither String (String, String, Int)))
    testBatch (applicative (undefined :: BahEither String (String, String, Int)))
    testBatch (monad (undefined :: BahEither String (String, String, Int)))
  
  describe "Constant" $ do
    testBatch (functor (undefined :: Constant String (String, String, Int)))
    testBatch (applicative (undefined :: Constant String (String, String, Int)))

  describe "Two" $ do
    testBatch (functor (undefined :: Two String (String, String, Int)))
    testBatch (applicative (undefined :: Two String (String, String, Int)))