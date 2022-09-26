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
import Types.Pair
import Types.Three
import Types.Four
import Types.Big
import Types.SkiFree
import Types.Tree
import Types.Moi
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
      let
        f = (+1) <$> (Moi $ \s -> (0, s))
      runMoi f 0 `shouldBe` (1,0)

  describe "Moi" $ do
    it "Sum" $ do
      (fmap (+1) $ read "[1]" :: [Int]) `shouldBe` [2]
    it "Sum" $ do
      fmap (fmap (++ "lol")) (Just ["Hi,", "Hello"]) `shouldBe` Just ["Hi,lol","Hellolol"]

  context "Functor Possibly'" $ do
    prop "Identity" (functorIdentity :: Possibly Int -> Bool)
    prop "Compose" (functorCompose (+1) (*2) ::  Possibly Int -> Bool)

  describe "MyList" $ do
    testBatch (monoid Twoo)

  describe "Identity" $ do
    testBatch (semigroup (undefined :: Identity String, 1::Int))
    testBatch (monoid (undefined :: Identity String))
    testBatch (functor (undefined :: Identity (String, String, Int)))
    testBatch (applicative (undefined :: Identity (String, String, Int)))
    testBatch (monad (undefined :: Identity (String, String, Int)))
    testBatch (foldable (undefined :: Identity (String, String, String, Int, String)))
    testBatch (traversable (undefined :: Identity (String, String, String)))

  describe "MyList" $ do
    testBatch (functor (undefined :: List (String, String, Int)))
    testBatch (applicative (undefined :: List (String, String, Int)))
    testBatch (monad (undefined :: List (String, String, Int)))
    testBatch (foldable (undefined :: List (String, String, String, Int, String)))
    testBatch (traversable (undefined :: List (String, String, String)))

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
    --testBatch (foldable (undefined :: Constant String (String, String, String, Int, String)))
    testBatch (traversable (undefined :: Constant String (String, String, String)))

  describe "Two" $ do
    testBatch (functor (undefined :: Two String (String, String, Int)))
    testBatch (applicative (undefined :: Two String (String, String, Int)))
    testBatch (foldable (undefined :: Two String (String, String, String, Int, String)))

  describe "Pair" $ do
    testBatch (functor (undefined :: Pair (String, String, Int)))
    testBatch (applicative (undefined :: Pair (String, String, Int)))
    testBatch (foldable (undefined :: Pair (String, String, String, Int, String)))
    testBatch (traversable (undefined :: Pair (String, String, String)))
  
  describe "Three" $ do
    testBatch (functor (undefined :: Three String String (String, String, Int)))
    testBatch (applicative (undefined :: Three String String (String, String, Int)))
    testBatch (foldable (undefined :: Three String String (String, String, String, Int, String)))

  describe "Three'" $ do
    testBatch (functor (undefined :: Three' String (String, String, Int)))
    testBatch (applicative (undefined :: Three' String (String, String, Int)))
    testBatch (foldable (undefined :: Three' String (String, String, String, Int, String)))

  describe "Four" $ do
    testBatch (functor (undefined :: Four String String String (String, String, Int)))
    testBatch (applicative (undefined :: Four String String String (String, String, Int)))
    testBatch (foldable (undefined :: Four String String String (String, String, String, Int, String)))

  describe "Four'" $ do
    testBatch (functor (undefined :: Four' String (String, String, Int)))
    testBatch (applicative (undefined :: Four' String (String, String, Int)))
    testBatch (foldable (undefined :: Four' String (String, String, String, Int, String)))

  describe "Big" $ do
    testBatch (functor (undefined :: Big String (String, String, Int)))
    testBatch (foldable (undefined :: Big String (String, String, String, Int, String)))
    testBatch (traversable (undefined :: Big String (String, String, String)))

  describe "Bigger" $ do
    testBatch (functor (undefined :: Bigger String (String, String, Int)))
    testBatch (foldable (undefined :: Bigger String (String, String, String, Int, String)))
    testBatch (traversable (undefined :: Bigger String (String, String, String)))

  describe "SkiFree" $ do
    testBatch (functor (undefined :: S [] (String, String, Int)))
    testBatch (foldable (undefined ::  S [] (String, String, String, Int, String)))
    testBatch (traversable (undefined ::  S [] (String, String, String)))

  describe "Tree" $ do
    testBatch (functor (undefined :: Tree (String, String, Int)))
    testBatch (foldable (undefined ::  Tree (String, String, String, Int, String)))
    testBatch (traversable (undefined :: Tree (String, String, String)))