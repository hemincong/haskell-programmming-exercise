import Test.QuickCheck(Arbitrary, arbitrary, quickCheck)
import BoolDisj
import Data.Semigroup(Semigroup, (<>))
import MonoidCheck
import SemigroupCheck

instance Arbitrary BoolDisj where 
    arbitrary = do
        a <- arbitrary
        return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

main :: IO()
main = do 
    quickCheck (semigroupAssoc :: BoolDisjAssoc ) 
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool ) 
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool) 
