module Main where

import Data.Semigroup

newtype Combine a b = Combine { unCombine :: ( a -> b)}

instance Semigroup b => Semigroup (Combine a b) where
   Combine { unCombine = f }  <> Combine { unCombine = g } = Combine (f <> g)

f = Combine $ \n -> Sum (n+1)
g = Combine $ \n -> Sum (n-1)

main :: IO()
main = do
    print $ unCombine ( f<>g) $ 0
    
