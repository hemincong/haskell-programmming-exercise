module Main where

import Control.Applicative
import Data.Monoid hiding (Sum, First)
import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b = First a | Second b deriving (Eq, Show)

data Validation e a = Error e | Success a deriving (Eq, Show)

--applyIfBothSeconed :: (Sum e) (a->b) -> (Sum e) a -> (Sum e) b

--applyMappendError:: Monoid e => (Validation e) (a->b) -> (Validation e) a -> (Validation e) b
--

instance Functor (Sum a) where
    fmap f (Second b) = Second (f b)
    fmap _ (First a) = First a

instance Applicative (Sum a) where
    pure = Second
    (<*>) (First a) (Second _) = First a
    (<*>) (First a) (First b) = First a
    (<*>) (Second a) (First b) = First b
    (<*>) (Second f) (Second a) = Second (f a)

instance Functor (Validation e) where
    fmap _ (Error e) = Error e
    fmap f (Success e) = Success (f e)

instance Monoid e => Applicative (Validation e) where
    pure = Success
    (<*>) (Error a) (Success _)= (Error a)
    (<*>) (Success _) (Error a)= (Error a)
    (<*>) (Success f) (Success b)= Success (f b)

genSum :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
genSum = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = genSum 

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = genValidation

genValidation :: (Arbitrary e, Arbitrary a) => Gen (Validation e a)
genValidation = do
    e <- arbitrary
    a <- arbitrary
    elements [Error e, Success a]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

type S = String

main :: IO()
main = do
    quickBatch (applicative (undefined :: Validation S (S, S, S)))
    quickBatch (applicative (undefined :: Sum S (S, S, S)))
