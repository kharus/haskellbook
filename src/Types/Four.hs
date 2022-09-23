module Types.Four where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four x y z w) = Four x y z (f w)

instance (Monoid a,  Monoid b,  Monoid c) => Applicative (Four a b c) where
    pure x = Four mempty mempty mempty x
    (Four a1 b1 c1 f) <*> (Four a2 b2 c2 x) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (f x)

instance Foldable (Four a b c) where
    foldMap f (Four _ _ _ d) = f d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data Four' a b = Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' x y z w) = Four' x y z (f w)

instance (Monoid a) => Applicative (Four' a) where
    pure x = Four' mempty mempty mempty x
    (Four' a1 b1 c1 f) <*> (Four' a2 b2 c2 x) = Four' (a1 <> a2) (b1 <> b2) (c1 <> c2) (f x)

instance Foldable (Four' a) where
    foldMap f (Four' _ _ _ d) = f d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq