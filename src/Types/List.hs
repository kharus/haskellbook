module Types.List where

import Test.QuickCheck.Checkers
import Test.QuickCheck
import Control.Applicative

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

instance Monad List where
    return = pure
    Nil >>= _ = Nil
    (Cons x xs) >>= f = append (f x) (xs >>= f)

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons x xs) = liftA2 Cons (f x) (traverse f xs)

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =  frequency [(1, return Nil), (5, fmap (`Cons` Nil) arbitrary)]
