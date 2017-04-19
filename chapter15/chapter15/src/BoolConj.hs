module BoolConj where

import Data.Semigroup(Semigroup, (<>))

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Snd b) <> _ = (Snd b)
    _ <> (Snd b) = (Snd b)
    (Fst a) <> (Fst b) = (Fst b)

