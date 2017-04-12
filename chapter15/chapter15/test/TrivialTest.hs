import Trivial
import Test.QuickCheck
import SemigroupCheck

instance Arbitrary Trivial where 
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO()
main = do 
    quickCheck (semigroupAssoc :: TrivialAssoc ) 
