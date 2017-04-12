module BoolConj where

import Data.Semigroup(Semigroup, (<>))

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    (BoolConj _) <> (BoolConj _) = BoolConj False

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False
    (BoolDisj _) <> (BoolDisj _) = BoolDisj True

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Snd b) <> _ = (Snd b)
    _ <> (Snd b) = (Snd b)
    (Fst a) <> (Fst b) = (Fst b)

