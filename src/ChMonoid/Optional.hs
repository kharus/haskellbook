module ChMonoid.Optional (module ChMonoid.Optional) where

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