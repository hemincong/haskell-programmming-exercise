module List1 where

import Control.Applicative
import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List1 a = Nil | Cons a (List1 a) deriving (Eq, Show)

take' :: Int -> List1 a -> List1 a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

instance Functor List1 where
    fmap _ Nil = Nil
    fmap f (Cons a l1) = Cons (f a) (fmap f l1)

instance Monoid (List1 a) where
    mempty = Nil
    mappend Nil a = a
    mappend a Nil = a
    mappend (Cons x xs) ys = Cons x (xs `mappend` ys)

instance Applicative List1 where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f b) ca = fmap f ca <> (b <*> ca)

instance Monad (List1) where
    return = pure
    (>>=) Nil _ = Nil
    (>>=) (Cons a b) f = (f a) <> (b >>= f)

instance Arbitrary a => Arbitrary (List1 a) where
    arbitrary = do 
        x <- arbitrary
        y <- arbitrary
        frequency [(1, return Nil), (10, return (Cons x y))]

instance Eq a => EqProp (List1 a) where
  xs =-= ys = xs' `eq` ys'
      where xs' = take' 3000 xs
            ys' = take' 3000 ys

functorTrigger = quickBatch ( functor ( undefined :: List1 (Int, String, Int)))
applicativeTrigger = quickBatch ( applicative ( undefined :: List1 (Int, String, Int)))
monadTrigger = quickBatch ( monad ( undefined :: List1 (Int, String, Int)))
