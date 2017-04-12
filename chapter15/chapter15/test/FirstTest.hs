import MonoidCheck 
import First
import Optional
import Test.QuickCheck

type FirstMappend = First' String -> First' String -> First' String -> Bool

instance Arbitrary a => Arbitrary(First' a) where
    arbitrary = do
        x <- arbitrary
        frequency [(1, return (First'(Only x)))
                    , (1, return (First' Nada))]

main :: IO()
main = do 
    quickCheck (monoidAssoc :: FirstMappend ) 
    quickCheck (monoidLeftIdentity :: First' String -> Bool ) 
    quickCheck (monoidRightIdentity :: First' String -> Bool ) 
