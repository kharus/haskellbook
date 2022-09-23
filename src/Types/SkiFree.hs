{-# LANGUAGE FlexibleContexts #-}
module Types.SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

data S n a = S (n a) a
    deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S x y) = S (fmap f x) (f y)

instance ( Functor n , Arbitrary (n a), Arbitrary a ) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n , Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
    (=-=) = eq

instance Foldable n => Foldable (S n) where
    foldMap f (S x y) = (foldMap f x) <> (f y)

instance Traversable n => Traversable (S n) where
    traverse f (S x y) = liftA2 S (traverse f x) (f y)