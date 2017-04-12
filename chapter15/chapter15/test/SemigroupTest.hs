import Test.QuickCheck(Arbitrary, arbitrary, quickCheck, frequency)
import BoolConj
import Data.Semigroup(Semigroup, (<>))
import SemigroupCheck

type OrMappend = Or String String -> Or String String -> Or String String -> Bool

instance (Arbitrary a , Arbitrary b)=> Arbitrary(Or a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [(1, return (Fst x))
                    , (1, return (Snd y))]

main :: IO()
main = do 
    quickCheck(semigroupAssoc :: OrMappend)
