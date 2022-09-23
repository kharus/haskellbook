module Types.Pair where

import Test.QuickCheck ( frequency, Arbitrary(arbitrary) )
import Test.QuickCheck.Checkers
import Control.Applicative (liftA2)

data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f1 f2) <*> (Pair x1 x2) = Pair (f1 x1) (f2 x2)

instance Foldable Pair where
    foldMap f (Pair x y) = f x <> f y

instance Traversable Pair where
    traverse f (Pair x y) = liftA2 Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance Eq a => EqProp (Pair a) where
    (=-=) = eq