module ChMonoid.Optional (module ChMonoid.Optional) where
import Test.QuickCheck ( frequency, Arbitrary(arbitrary) )
data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup  a => Semigroup (Optional a) where
    (Only x) <> Nada = Only x
    Nada <> (Only x) = Only x
    (Only x) <> (Only y)= Only (x <> y)
    _ <> _ = Nada

instance Monoid a => Monoid (Optional a) where
    mempty = Only mempty

data Bull = Fools | Twoo deriving (Eq, Show)

instance Semigroup Bull where
    (<>) Fools Fools = Fools
    (<>) _ _ = Twoo

instance Monoid Bull where
    mempty = Fools

newtype First' a = First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Semigroup (First' a) where
    (First' x) <> (First' Nada) = First' x
    (First' Nada) <> (First' x) = First' x
    (First' x) <> (First' _)=  First' x

instance Monoid (First' a) where
    mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools) , (1, return Twoo) ]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    frequency [ (1, return (First' Nada)), (1, return (First' (Only x)))]

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    Trivial <> Trivial = Trivial

instance Monoid Trivial where
    mempty = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity $ x <> y

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    Identity <$> arbitrary


newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _ = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True
    
instance Arbitrary BoolConj where
  arbitrary = frequency [ (1, return $ BoolConj False) , (1, return $ BoolConj True) ]

newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False
    _ <> _ = BoolDisj True

instance Monoid BoolDisj where
    mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = frequency [ (1, return $ BoolDisj False) , (1, return $ BoolDisj True) ]

data Or a b = Fst a | Snd b
    deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Fst _) <> (Fst y) = Fst y
    (Fst _) <> (Snd y) = Snd y
    (Snd x) <> _ = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return (Fst x)), (1, return (Snd y))]

newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

instance Arbitrary b => Arbitrary (Combine a b) where
  arbitrary = do
    Combine . const <$> arbitrary

newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup a => Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp $ f.g

instance Arbitrary a => Arbitrary (Comp a) where
  arbitrary = do
    Comp . const <$> arbitrary

data Validation a b = Failure a | Success b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Failure x) <> (Failure y) = Failure $ x <> y
    (Success x) <> _ = Success x
    _ <> (Success y) = Success y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return (Failure x)), (3, return (Success y))]

newtype Mem s a = Mem
    { runMem :: s -> (a,s)
    }

instance Semigroup a => Semigroup (Mem s a) where
    (Mem f) <> (Mem g) = Mem (\s ->
        let
            (ga,s1) = g s
            (fa,s2) = f s1
        in (fa <> ga, s2))

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
