module Types.BahEither where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data BahEither b a = PLeft a | PRight b
    deriving (Eq, Show)

instance Functor (BahEither b) where
    fmap f (PLeft x) = PLeft (f x)
    fmap _ (PRight x) = PRight x

instance Applicative (BahEither b) where
    pure = PLeft
    (PRight f) <*> _ = PRight f
    (PLeft _) <*> (PRight x) = PRight x
    (PLeft f) <*> (PLeft x) = PLeft (f x)

instance Monad (BahEither b) where
    return = pure
    (PLeft x) >>= f = f x
    (PRight x) >>= _ = PRight x

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = frequency [(1, fmap PRight arbitrary), (1, fmap PLeft arbitrary)]


instance (Eq b, Eq a) => EqProp (BahEither b a) where
    (=-=) = eq
