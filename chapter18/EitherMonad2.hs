module EitherMonad where

import Control.Applicative
import Data.Monoid (Monoid, (<>))
import Test.QuickCheck (Arbitrary, arbitrary, elements, frequency)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative, monad)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a ) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure  = Second
    (<*>) (First a) _ = First a
    (<*>) _ (First b) = First b
    (<*>) (Second f) (Second x)= Second (f x)

instance Monad (Sum a) where
    return = pure
    (>>=) (First a) _ = First a
    (>>=) (Second a) f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return (First a)), (2, return (Second b))]

instance (Eq a, Eq b) => EqProp (Sum a b) where 
    (=-=) = eq

-- quickBatch $ applicative (Second ("b", "w", 1))
-- quickBatch $ monad (Second ("b", "w", 1))
