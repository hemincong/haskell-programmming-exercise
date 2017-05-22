module Lib where

import Data.Functor

newtype Identity1 a = Identity1 a deriving (Eq, Show)

data Pair1 a = Pair1 a a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

data Three1 a b = Three1 a b b deriving (Eq, Show)

data Four a b c d = Four a b c d deriving (Eq, Show)

data Four1 a b = Four1 a a a b deriving (Eq, Show)

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

data Sum1 a b = First a | Second b deriving (Eq, Show)

data More b a = L a b a | R b a b deriving (Eq, Show)

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

data K a b = K a deriving (Eq, Show)

instance Functor Identity1 where
    fmap f (Identity1 a) = Identity1 (f a)

instance Functor Pair1 where
    fmap f (Pair1 a b) = (Pair1 (f a) (f b))

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Functor (Three1 a)  where
    fmap f (Three1 a b c) = Three1 a (f b) (f c)

instance Functor (Four a b c)  where
    fmap f (Four a b c d) = Four a b c (f d)

instance Functor (Four1 a)  where
    fmap f (Four1 a b c d) = Four1 a b c (f d)

instance Functor Possibly where
    fmap f (LolNope) =  LolNope
    fmap f (Yeppers a) = Yeppers (f a)

instance Functor (Sum1 a) where
    fmap _ (First a) = First a
    fmap f (Second a) = Second (f a)

instance Functor (More a) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

instance Functor (K a) where
    fmap _ (K a) = K a
