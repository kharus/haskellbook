{-# LANGUAGE InstanceSigs #-}
module Types.Three where
import Test.QuickCheck
import Control.Applicative
import Test.QuickCheck.Checkers

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance (Monoid a,  Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    (Three a1 b1 f) <*> (Three a2 b2 x) = Three (a1 <> a2) (b1 <> b2) (f x)

instance Foldable (Three a b) where
    foldMap f (Three _ _ b) = f b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a) => Applicative (Three' a) where
    pure x = Three' mempty x x
    (Three' a1 f1 f2) <*> (Three' a2 b2 c2) = Three' (a1 <> a2) (f1 b2) (f2 c2)

instance Foldable (Three' a) where
    foldMap f (Three' _ b c) = (f b) <> (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq