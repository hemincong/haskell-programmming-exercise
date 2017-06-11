module Main where

import Data.Monoid 
import Control.Applicative 

data List1 a = Nil | Cons a (List1 a) deriving (Eq, Show)

append :: List1 a -> List1 a -> List1 a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

fold :: (a -> b -> b) -> b -> List1 a -> b
fold _ b Nil = b
fold f b (Cons x xs) = f x (fold f b xs)

concat' :: List1 (List1 a) -> List1 a
concat' = fold append Nil

instance Monoid (List1 a) where
    mempty = Nil
    mappend a Nil = a
    mappend Nil a = a
    mappend (Cons x xs) ys = Cons x (xs `mappend` ys)

instance Functor List1 where
    fmap _ Nil = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List1 where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f b1) xs = fmap f xs `append` (b1 <*> xs)

functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)

main :: IO()
main = putStrLn $ show $ functions <*> values

