module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure x = NopeDotJpg
    (<*>) _ _ = NopeDotJpg
    
instance Monad Nope where
    return = pure
    NopeDotJpg >>= f = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

data PhhhbbtttEither b a = PLeft a | PRight b deriving (Show, Eq)

instance Functor (PhhhbbtttEither b) where
    fmap f (PLeft a) = PLeft (f a)
    fmap _ (PRight b) = PRight b

instance Applicative (PhhhbbtttEither b) where
    pure = PLeft
    (<*>) _ (PRight a) = PRight a
    (<*>) (PRight a) _ = PRight a
    (<*>) (PLeft f) (PLeft b) = PLeft (f b)

instance Monad (PhhhbbtttEither a) where
    return = pure
    (>>=) (PRight a) _ = PRight a
    (>>=) (PLeft a) f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
    arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [PLeft b, PRight a]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where (=-=) = eq

main = do
    let trigger = undefined :: Nope (Int, String, Int)
    let trigger2 = undefined :: PhhhbbtttEither String (Int, String, Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
    quickBatch $ functor trigger2
    quickBatch $ applicative trigger2
    quickBatch $ monad trigger2
