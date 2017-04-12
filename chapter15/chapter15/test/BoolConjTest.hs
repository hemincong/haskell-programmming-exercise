import Test.QuickCheck(Arbitrary, arbitrary, quickCheck)
import BoolConj
import Data.Semigroup(Semigroup, (<>))

main :: IO()
main = do 
    putStrLn $ show $ (BoolConj True) <> (BoolConj True)
