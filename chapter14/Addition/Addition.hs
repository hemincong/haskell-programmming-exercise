module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
        where go n d count 
                | n < d = (count, n)
                | otherwise = go (n - d) d (count+1)

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genTuple :: (Arbitrary a, Arbitrary b) => Gen(a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genEither :: (Arbitrary a, Arbitrary b) => Gen(Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

main :: IO()
main = hspec $ do
        describe "Addition" $ do
            it "1+1 is greater than 1" $ do
                (1+1) > 1 `shouldBe` True
        describe "dividedBy" $ do
            it "15 divided by 3 is 5" $ do
                dividedBy 15 3 `shouldBe` (5, 0)
            it "22 divided by 5 is 4" $ do
                dividedBy 22 5 `shouldBe` (4, 2)
            it "x+1 is always greater than x" $ do
                property $ \x -> x + 1 > (x::Int)



