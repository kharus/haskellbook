{-# LANGUAGE InstanceSigs #-}
module Types.Big where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

data Big a b = Big a b b
    deriving (Eq, Show)

instance Functor (Big a) where
    fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
    foldMap f (Big _ y z) = f y <> f z

instance Traversable (Big a) where
    traverse f (Big x y z) = liftA2 (Big x) (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = liftA3 Big arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq

data Bigger a b = Bigger a b b b
   deriving (Eq, Show)

instance Functor (Bigger a) where
    fmap f (Bigger x y z w) = Bigger x (f y) (f z) (f w)

instance Foldable (Bigger a) where
    foldMap f (Bigger _ y z w) = f y <> f z <> f w

instance Traversable (Bigger a) where
    traverse f (Bigger x y z w) = liftA3 (Bigger x) (f y) (f z) (f w)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
    (=-=) = eq
