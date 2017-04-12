import Test.QuickCheck(Arbitrary, arbitrary, quickCheck, frequency)
import Validation
import Data.Semigroup(Semigroup, (<>))
import SemigroupCheck

type ValidationString = Validation String String -> Validation String String -> Validation String String -> Bool

instance (Arbitrary a , Arbitrary b)=> Arbitrary(Validation a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [(1, return (Failure x))
                    , (1, return (Success y))]

type AccmulateBothString = AccmulateBoth String String -> AccmulateBoth String String -> AccmulateBoth String String -> Bool

instance (Arbitrary a , Arbitrary b)=> Arbitrary(AccmulateBoth a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [(1, return (AccmulateBoth(Failure x)))
                    , (1, return (AccmulateBoth(Success y)))]

type AccmulateRightString = AccmulateRight String String -> AccmulateRight String String -> AccmulateRight String String -> Bool

instance (Arbitrary a , Arbitrary b)=> Arbitrary(AccmulateRight a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [(1, return (AccmulateRight(Failure x)))
                    , (1, return (AccmulateRight(Success y)))]

main :: IO()
main = do 
    quickCheck(semigroupAssoc :: ValidationString)
    quickCheck(semigroupAssoc :: AccmulateBothString)
    quickCheck(semigroupAssoc :: AccmulateRightString)
