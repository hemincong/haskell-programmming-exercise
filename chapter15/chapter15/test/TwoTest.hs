import Test.QuickCheck(Arbitrary, arbitrary, quickCheck)
import SemigroupCheck
import Two
import MonoidCheck

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where 
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

main :: IO()
main = do 
    quickCheck (semigroupAssoc :: TwoAssoc ) 
    quickCheck (monoidLeftIdentity :: Two String String -> Bool ) 
    quickCheck (monoidRightIdentity :: Two String String -> Bool) 
