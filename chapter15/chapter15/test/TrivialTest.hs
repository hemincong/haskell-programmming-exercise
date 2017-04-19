import Trivial
import Test.QuickCheck
import SemigroupCheck
import MonoidCheck

instance Arbitrary Trivial where 
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO()
main = do 
    quickCheck (semigroupAssoc :: TrivialAssoc ) 
    quickCheck (monoidLeftIdentity :: Trivial-> Bool ) 
    quickCheck (monoidRightIdentity :: Trivial-> Bool) 
