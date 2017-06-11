module Main where

import Control.Applicative 
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List1 a = Nil | Cons a (List1 a) deriving (Eq, Show)

append :: List1 a -> List1 a -> List1 a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

take1 :: Int -> List1 a -> List1 a
take1 _ Nil = Nil
take1 0 (Cons x xs) = (Cons x Nil)
take1 n (Cons x xs) = (Cons x Nil)`append` (take1 (n-1) xs)

instance Functor List1 where
    fmap _ Nil = Nil
    fmap f (Cons a xs) = Cons (f a) (fmap f xs)

instance Applicative List1 where
    pure a = Cons a Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f b1) xs = fmap f xs `append` (b1 <*> xs)

newtype ZipList1 a = ZipList1 (List1 a) deriving (Eq, Show)

--toList1 :: [a] -> List1 a
--toList1 = foldr Cons Nil

--instance Arbitrary a => Arbitrary (List1 a) where
    --arbitrary = fmap toList1 arbitrary

instance Eq a => EqProp (ZipList1 a) where
    xs =-= ys = xs1 `eq` ys1
        where xs1 = let (ZipList1 l) = xs
                    in take1 3000 l 
              ys1 = let (ZipList1 l) = ys
                    in take1 3000 l 

instance Monoid a => Monoid (ZipList1 a) where
    mempty = ZipList1 Nil
    mappend = liftA2 mappend

    

instance Arbitrary a => Arbitrary (List1 a) where
    arbitrary = genList1

instance Arbitrary a => Arbitrary (ZipList1 a) where
    arbitrary = genZipList

genList1 :: Arbitrary a => Gen (List1 a)
genList1 = do
    h <- arbitrary
    t <- genList1
    frequency [(3, return $ Cons h t),
           (1, return Nil)]

genZipList :: Arbitrary a => Gen (ZipList1 a)
genZipList = do
    l <- arbitrary
    return (ZipList1 l)
                    
instance Functor ZipList1 where
    fmap _ (ZipList1 Nil) = ZipList1 Nil
    fmap f (ZipList1 l1) = ZipList1 (fmap f l1)

instance Applicative ZipList1 where
    pure a = ZipList1 (Cons a Nil)
    (<*>) (ZipList1 l1) (ZipList1 l2) = ZipList1 (l1 <*> l2)

main :: IO()
main = quickBatch (applicative $ ZipList1 (Cons (undefined :: (Bool, Bool, Bool)) Nil))
