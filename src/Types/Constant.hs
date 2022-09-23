module Types.Constant where
import Test.QuickCheck
import Test.QuickCheck.Checkers

newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant x)= Constant x

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant a) <*> (Constant b) = Constant (a<>b)

instance Foldable (Constant a) where
    foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
    traverse _ (Constant a) = pure $ Constant a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = do
    Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
    (=-=) = eq