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

data Three1 a b = Three1 a b b deriving (Eq, Show)

instance Functor (Three1 a) where
    fmap f (Three1 a b1 b2) = Three1 a (f b1) (f b2)

instance Monoid a => Applicative (Three1 a) where
    pure b = Three1 mempty b b
    (<*>) (Three1 a1 f1 f2) (Three1 a2 b1 b2) = Three1 (a1<>a2) (f1 b1) (f2 b2)

genThree1 :: (Arbitrary a, Arbitrary b) => Gen (Three1 a b)
genThree1 = do
    a <- arbitrary
    b <- arbitrary
    return (Three1 a b b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three1 a b) where
    arbitrary = genThree1

instance (Eq a, Eq b) => EqProp (Three1 a b ) where
    (=-=) = eq

testThree1Prop :: IO()
testThree1Prop = quickBatch(applicative (undefined :: Three String String (String, String, String)))

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c ) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (<*>) (Four a1 b1 c1 f1) (Four a2 b2 c2 d2) = Four (a1<>a2) (b1 <> b2) (c1 <> c2) (f1 d2)

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = genFour

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

testFourProp :: IO()
testFourProp = quickBatch(applicative (undefined :: Four String String String (String, String, String)))


data Four1 a b = Four1 a a a b deriving (Eq, Show)

instance Functor (Four1 a) where
    fmap f (Four1 a1 a2 a3 b)  = Four1 a1 a2 a3 (f b)

instance (Monoid a) => Applicative (Four1 a) where
    pure = Four1 mempty mempty mempty
    (<*>) (Four1 a1 a2 a3 f1) (Four1 a4 a5 a6 b1) = Four1 (a1<>a4) (a2<>a5) (a3<>a6) (f1 b1)

genFour1 :: (Arbitrary a, Arbitrary b) => Gen (Four1 a b)
genFour1 = do
    a <- arbitrary
    b <- arbitrary
    return (Four1 a a a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four1 a b) where
    arbitrary = genFour1

instance (Eq a, Eq b) => EqProp (Four1 a b) where
    (=-=) = eq

testFour1Prop :: IO()
testFour1Prop = quickBatch(applicative (undefined :: Four1 String (String, String, String)))
