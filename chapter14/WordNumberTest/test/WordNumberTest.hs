module Main where

import Test.Hspec
import Test.QuickCheck (quickCheck)
import Test.QuickCheck (sample, oneof, frequency, Gen)
import Cipher

--import WordNumber

--main :: IO ()
--main = hspec $ do 
--    describe "digitToWord" $ do
--        it "returns zero for 0" $ do
--            digitToWord 0 `shouldBe` "zero"
--        it "returns one for 1" $ do
--            digitToWord 1 `shouldBe` "one"
--
--    describe "digits" $ do
--        it "returns [1] for 1" $ do
--            digits 1 `shouldBe` [1]
--        it "returns [1,0,0] for 100" $ do
--            digits 100 `shouldBe` [1, 0, 0]
--
--    describe "wordNumber" $ do
--        it "one-zero-zero given 100" $ do
--            wordNumber 100 `shouldBe` "one-zero-zero"
--        it "nine-zero-zero-one given 9001" $ do
--            wordNumber 9001 `shouldBe` "nine-zero-zero-one"
--

import UsingQuickCheck

data Fool =
      Fulse
      | Frue
      deriving (Eq, Show)

genFool :: Gen Fool
genFool = oneof [return $ Fulse, return $ Frue]

genFoolFreq :: Gen Fool
genFoolFreq = frequency [(2, return Fulse),
                            (1, return Frue)]


caesarProp :: Int -> String -> Bool
caesarProp n s = (unCaesar n (caesar n s)) == s

main :: IO()
main = do
    quickCheck propHalf
    quickCheck porpSort
    quickCheck plusAssociative
    quickCheck plusCommutative
    quickCheck multiAssociative
    quickCheck multiCommutative
    quickCheck propQuotRem
    quickCheck powerAssociative
    quickCheck powerCommutative
    quickCheck propReverse
    quickCheck composeProp
    quickCheck inConcatProp
    quickCheck concatProp
    quickCheck lengthTakeProp
    quickCheck readShowProp
    quickCheck squareProp
    quickCheck capitalizeWordProp
    quickCheck sortProp
    quickCheck caesarProp
    --sample genFool
    --sample genFoolFreq


