import Trivial
import Test.QuickCheck(Arbitrary, arbitrary, quickCheck)
import SemigroupCheck
import MonoidCheck
import Identity

instance (Arbitrary a) => Arbitrary (Identity a) where 
    arbitrary = do
        a <- arbitrary
        return (Identity a)

type IdentityAssoc = Identity String  -> Identity String -> Identity String -> Bool

main :: IO()
main = do 
    quickCheck (semigroupAssoc :: IdentityAssoc ) 
    quickCheck (monoidRightIdentity:: Identity String -> Bool ) 
    quickCheck (monoidLeftIdentity:: Identity String -> Bool ) 
