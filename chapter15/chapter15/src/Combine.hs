module Combine where

import Data.Monoid(Monoid)
import Data.Semigroup

newtype Combine a b = Combine { unCombine :: ( a -> b)}

instance Semigroup b => Semigroup (Combine a b) where
   Combine { unCombine = f }  <> Combine { unCombine = g } = Combine (f <> g)

instance (Semigroup b, Monoid b) =>  Monoid (Combine a b) where
    mempty = Combine mempty 
    mappend = (<>)

f = Combine $ \n -> Sum (n+1)
g = Combine $ \n -> Sum (n-1)

main :: IO()
main = do
    print $ unCombine ( f<>g) $ 0
    print $ unCombine (mappend f mempty ) $ 1
    
