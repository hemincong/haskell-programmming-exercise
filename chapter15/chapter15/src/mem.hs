module Main where

import Data.Monoid

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
    mempty = undefined
    mappend = undefined
    
f' = Mem $ \s -> ("hi", s+1)

main :: IO()
main = putStrLn "hello world"
