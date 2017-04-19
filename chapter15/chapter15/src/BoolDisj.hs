module BoolDisj where

import Data.Semigroup(Semigroup, (<>))
import Data.Monoid(Monoid)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)
