{-# LANGUAGE InstanceSigs #-}

module Types.Compose where

newtype Identity a =
    Identity { runIdentity :: a }

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ (pure.pure) a

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) =  Compose $ fmap (<*>) f <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose a) = (foldMap.foldMap) f a

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f  (Compose fga) = Compose <$> (traverse.traverse) f fga

xs :: [Maybe Integer]
xs = [Just 1, Nothing]

-- >>> fmap (+1) (Compose xs)
-- Compose {getCompose = [Just 2,Nothing]}
