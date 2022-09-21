module Types.Identity (module Types.Identity) where
import Test.QuickCheck
import Test.QuickCheck.Checkers

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity $ x <> y

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
    return = pure
    (Identity x) >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    Identity <$> arbitrary


instance Eq a => EqProp (Identity a) where
    (=-=) = eq
