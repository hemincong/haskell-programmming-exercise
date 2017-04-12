import Test.QuickCheck(Arbitrary, arbitrary, quickCheck)
import SemigroupCheck
import Three

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

main :: IO()
main = do 
    quickCheck (semigroupAssoc :: ThreeAssoc ) 
