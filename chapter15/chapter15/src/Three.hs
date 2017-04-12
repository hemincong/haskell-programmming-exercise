module Three where

import Data.Semigroup(Semigroup, (<>))

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three x y z) = Three (a <> x) (b <> y) (c <> z)
