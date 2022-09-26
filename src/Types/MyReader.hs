{-# LANGUAGE InstanceSigs #-}
module Types.MyReader where

import Control.Applicative
import Data.Maybe

newtype MyReader r a =
    MyReader { runReader :: r -> a }

instance Functor (MyReader r) where
    fmap :: (a -> b) -> MyReader r a -> MyReader r b
    fmap f (MyReader ra) = MyReader $ \r -> f (ra r)

instance Applicative (MyReader r) where
    pure :: a -> MyReader r a
    pure a = MyReader $ const a

    (<*>) :: MyReader r (a -> b) -> MyReader r a -> MyReader r b
    (MyReader rab) <*> (MyReader ra) = MyReader $ \r -> (rab r) (ra r)

instance Monad (MyReader r) where
    return = pure
    (>>=) :: MyReader r a -> (a -> MyReader r b) -> MyReader r b
    (MyReader ra) >>= aRb = MyReader $ \r -> runReader (aRb (ra r)) r

-- same as (.)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

ask :: MyReader a a
ask = MyReader id

newtype HumanName = HumanName String
    deriving (Eq, Show)

newtype DogName = DogName String
    deriving (Eq, Show)

newtype Address = Address String
    deriving (Eq, Show)

data Person = Person {
    humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    } deriving (Eq, Show)

data Dog = Dog {
    dogsName :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

pers :: Person
pers =
  Person
    (HumanName "Big Bird")
    (DogName "Barkley")
    (Address "Sesame Street")

chris :: Person
chris =
  Person
    (HumanName "Chris Allen")
    (DogName "Papu")
    (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

asks :: (r -> a) -> MyReader r a
asks = MyReader

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]


xs :: Maybe Integer
xs = lookup 3 (zip x y)

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x z

z' :: Integer -> Maybe Integer
z' n = lookup n (zip x z)

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)