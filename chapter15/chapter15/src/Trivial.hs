module Trivial where

import Data.Semigroup(Semigroup, (<>))
import Data.Monoid(Monoid)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)
