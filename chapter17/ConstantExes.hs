module ConstantExes where

import Data.Monoid

newtype Constant a b = Constant { getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a ) = Constant a 

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant { getConstant = mempty }
    a <*> a1 = Constant (getConstant a <> getConstant a1)
