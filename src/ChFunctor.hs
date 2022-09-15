module ChFunctor (module ChFunctor) where
import Test.QuickCheck
import Control.Monad

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    Identity <$> arbitrary

data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = liftM2 Pair arbitrary arbitrary

data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM2 Two arbitrary arbitrary

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftM3 Three arbitrary arbitrary arbitrary

data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftM3 Three' arbitrary arbitrary arbitrary

data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four x y z w) = Four x y z (f w)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = liftM4 Four arbitrary arbitrary arbitrary arbitrary

data Four' a b = Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' x y z w) = Four' x y z (f w)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = liftM4 Four' arbitrary arbitrary arbitrary arbitrary

data Possibly a = LolNope | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers $ f a

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary =  frequency [(1, return LolNope), (3, fmap Yeppers arbitrary)]