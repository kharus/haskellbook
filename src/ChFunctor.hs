{-# LANGUAGE InstanceSigs #-}
module ChFunctor (module ChFunctor) where
import Test.QuickCheck
import Control.Applicative
import Test.QuickCheck.Checkers (EqProp, (=-=), eq)
import Control.Monad (join)

data Possibly a = LolNope | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers $ f a

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = frequency [(1, return LolNope), (3, fmap Yeppers arbitrary)]

data Bull = Fools | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]

instance Semigroup Bull where
    (<>) Fools Fools = Fools
    (<>) _ _ = Twoo

instance Monoid Bull where
    mempty = Fools

-- EqProp is from the checkers library
instance EqProp Bull where
    (=-=) = eq

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

data Sum a b = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (Second x) = Second (f x)
    fmap _ (First y) = First y

instance Applicative (Sum a) where
    pure = Second
    (Second f) <*> (Second x) = Second (f x)
    (First f) <*> _ = First f
    _ <*> (First x) = First x

instance Monad (Sum a) where
    return = pure
    (First x) >>= _ = First x
    (Second x) >>= f = f x

instance (Eq a,Eq b) => EqProp (Sum a b) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary =  frequency [(1, fmap First arbitrary), (1, fmap Second arbitrary)]

data Nope a = NopeDotJpg
    deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) :: Nope (a -> b) -> Nope a -> Nope b
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    return = pure
    _ >>= _ = NopeDotJpg

instance (Eq a) => EqProp (Nope a) where
    (=-=) = eq

instance (Arbitrary a) => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

j :: Monad m => m (m a) -> m a
j m = do
    m2 <- m
    m2

filterF :: ( Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

filterFq :: (Bool -> Bool) -> [Bool] -> [Bool]
filterFq = filterF