{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
module ChFunctorSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (quickCheck)
import ChFunctor

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

  context "Functor List" $ do
    prop "Identity" (functorIdentity :: [Int] -> Bool)
    prop "Compose" (functorCompose (+1) (*2) :: [Int] -> Bool)

  context "Functor Identity" $ do
    prop "Identity" (functorIdentity :: Identity Int -> Bool)
    prop "Compose" (functorCompose (+1) (*2) :: Identity Int -> Bool)