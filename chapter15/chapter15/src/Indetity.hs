module Id where

import Data.Semigroup(Semigroup, (<>))

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial
