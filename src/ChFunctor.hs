{-# LANGUAGE InstanceSigs #-}
module ChFunctor (module ChFunctor) where
import Test.QuickCheck
import Control.Applicative
import Test.QuickCheck.Checkers (EqProp, (=-=), eq)
import Control.Monad (join)

data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = liftA2 Pair arbitrary arbitrary

data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four x y z w) = Four x y z (f w)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data Four' a b = Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' x y z w) = Four' x y z (f w)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data Possibly a = LolNope | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers $ f a

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = frequency [(1, return LolNope), (3, fmap Yeppers arbitrary)]

newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant x)= Constant x

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant a) <*> (Constant b) = Constant (a<>b)

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

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =  frequency [(1, return Nil), (5, fmap (`Cons` Nil) arbitrary)]

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