module ChapterExes where

import Control.Applicative
import Data.Monoid hiding (Sum, First)
import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure a = Identity a
    (<*>) (Identity f) (Identity a) = Identity (f a)

genIdentity :: (Arbitrary a) => Gen (Identity a)
genIdentity = do
    a <- arbitrary
    return (Identity a)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = genIdentity

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

testIdentityProp :: IO()
testIdentityProp = quickBatch(applicative (undefined :: Identity (String, String, String)))

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair a a1) = Pair (f a) (f a1)

instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair f1 f2) (Pair a1 a2) = Pair (f1 a1) (f2 a2)

genPair :: (Arbitrary a) => Gen (Pair a)
genPair = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = genPair

instance (Eq a) => EqProp (Pair a) where
    (=-=) = eq

testPairProp :: IO()
testPairProp = quickBatch(applicative (undefined :: Pair (String, String, String)))

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    (<*>) (Two m f) (Two m1 x1)= Two (m<>m1) (f x1)
    
genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = genTwo

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq
    
testTwoProp :: IO()
testTwoProp = quickBatch(applicative (undefined :: Two String (String, String, String)))

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f ( Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure a = Three mempty mempty a
    (<*>) (Three a1 b1 f) (Three a2 b2 x) = Three (a1 <> a2) (b1 <> b2) (f x)

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = genThree

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

testThreeProp :: IO()
testThreeProp = quickBatch(applicative (undefined :: Three String String (String, String, String)))
