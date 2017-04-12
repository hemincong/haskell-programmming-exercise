import Test.QuickCheck(Arbitrary, arbitrary, quickCheck)
import SemigroupCheck
import Four

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where 
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (Four a b c d)

type FourAssoc = Four String String String String -> Four String String String String -> Four String String String String -> Bool

main :: IO()
main = do 
    quickCheck (semigroupAssoc :: FourAssoc ) 
