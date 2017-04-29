module Lib where

import Data.Functor

newtype Identity1 a = Identity1 a deriving (Eq, Show)

data Pair1 a = Pair1 a a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

data Three1 a b = Three1 a b b deriving (Eq, Show)

data Four a b c d = Four a b c d deriving (Eq, Show)

data Four1 a b = Four1 a a a b deriving (Eq, Show)

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
