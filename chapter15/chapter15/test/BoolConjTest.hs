import Test.QuickCheck(Arbitrary, arbitrary, quickCheck)
import BoolConj
import Data.Semigroup(Semigroup, (<>))

instance Arbitrary BoolConj where 
    arbitrary = do
        a <- arbitrary
        return BoolConj a

type BoolConjAssoc = BoolConj -> BoolConj Bool -> BoolConj Bool  -> Bool

main :: IO()
main = do 
    quickCheck (semigroupAssoc :: BoolConjAssoc ) 
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool ) 
    quickCheck (monoidRightIdentity :: BoolConj -> Bool) 
