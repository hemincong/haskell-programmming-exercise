{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

import Lib

-- checker

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

type IntID = [Int] -> Bool

-- identity1

identity1Gen :: (Arbitrary a) => Gen (Identity1 a)
identity1Gen = do
    a <- arbitrary
    return (Identity1 a)

instance (Arbitrary a ) => Arbitrary (Identity1 a) where
    arbitrary = identity1Gen

type Identity1String = Identity1 String -> Bool

type Identity1FC = Identity1 Int -> IntToInt -> IntToInt -> Bool

-- pair

pair1Gen :: (Arbitrary a) => Gen (Pair1 a)
pair1Gen = do
    a <- arbitrary
    return (Pair1 a a)

instance (Arbitrary a ) => Arbitrary (Pair1 a) where
    arbitrary = pair1Gen

type Pair1String = Pair1 String -> Bool

type Pair1FC = Pair1 Int -> IntToInt -> IntToInt -> Bool

-- two 

twoGen :: (Arbitrary a , Arbitrary b) => Gen (Two a b)
twoGen = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b ) => Arbitrary (Two a b) where
    arbitrary = twoGen

type TwoString = Two String String -> Bool

type TwoFC = Two Int Int-> IntToInt -> IntToInt -> Bool

-- three

threeGen :: (Arbitrary a , Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = threeGen

type ThreeString = Three String String String -> Bool

type ThreeFC = Three Int Int Int-> IntToInt -> IntToInt -> Bool

-- Three1

three1Gen :: (Arbitrary a , Arbitrary b) => Gen (Three1 a b )
three1Gen = do
    a <- arbitrary
    b <- arbitrary
    return (Three1 a b b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three1 a b ) where
    arbitrary = three1Gen

type Three1String = Three1 String String -> Bool

type Three1FC = Three1 Int Int -> IntToInt -> IntToInt -> Bool

-- Four

fourGen :: (Arbitrary a , Arbitrary b, Arbitrary c , Arbitrary d) => Gen (Four a b c d)
fourGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Arbitrary a , Arbitrary b, Arbitrary c , Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = fourGen

type FourString = Four String String String String -> Bool

type FourFC = Four Int Int Int Int-> IntToInt -> IntToInt -> Bool

-- Four1

four1Gen :: (Arbitrary a , Arbitrary b) => Gen (Four1 a b)
four1Gen = do
    a <- arbitrary
    b <- arbitrary
    return (Four1 a a a b)

instance (Arbitrary a , Arbitrary b) => Arbitrary (Four1 a b) where
    arbitrary = four1Gen

type Four1String = Four1 String String -> Bool

type Four1FC = Four1 Int Int-> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do 
    quickCheck (functorCompose  :: IntFC)
    quickCheck (functorIdentity :: IntID)
    quickCheck (functorCompose  :: Identity1FC)
    quickCheck (functorIdentity :: Identity1String)
    quickCheck (functorCompose  :: Pair1FC)
    quickCheck (functorIdentity :: Pair1String)
    quickCheck (functorCompose  :: TwoFC)
    quickCheck (functorIdentity :: TwoString)
    quickCheck (functorCompose  :: ThreeFC)
    quickCheck (functorIdentity :: ThreeString)
    quickCheck (functorCompose  :: Three1FC)
    quickCheck (functorIdentity :: FourString)
    quickCheck (functorCompose  :: FourFC)
    quickCheck (functorIdentity :: FourString)
    quickCheck (functorCompose  :: Four1FC)
    quickCheck (functorIdentity :: Four1String)
