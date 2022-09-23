module Types.Tree where
import Control.Applicative
import Test.QuickCheck.Checkers
import Test.QuickCheck

data Tree a =
    Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a ) = Leaf (f a)
    fmap f (Node lt a rt) = Node (fmap f lt) (f a) (fmap f rt)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node lt a rt) = (foldMap f lt) <> (f a) <> (foldMap f rt)

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node lt a rt) = liftA3 Node (traverse f lt) (f a) (traverse f rt)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency [(1, return Empty), (3, fmap Leaf arbitrary), (3, liftA3 Node (Leaf <$> arbitrary) arbitrary (Leaf <$> arbitrary))]

instance Eq a => EqProp (Tree a) where
    (=-=) = eq