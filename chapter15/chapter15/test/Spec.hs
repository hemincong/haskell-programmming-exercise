import Optional
import MonoidCheck

import Data.Monoid
import Test.Hspec
import Test.QuickCheck

type S = String
type B = Bool

main :: IO ()
main = do 
    quickCheck(monoidAssoc :: S -> S -> S -> B)
    quickCheck(monoidRightIdentity :: S -> B)
    quickCheck(monoidLeftIdentity :: S -> B)

--main = hspec $ do
--    describe "Optional" $ do
--        it "Only (Sum 1) `mappend` Only (Sum 1)" $ do
--            Only (Sum 1) `mappend` Only (Sum 1) `shouldBe` Only(Sum {getSum = 2})
--        it "Only (Product 4) `mappend` Only (Product 2)" $ do
--            Only (Product 4) `mappend` Only (Product 2) `shouldBe` Only(Product {getProduct = 8})
--        it "Only (Sum 1) `mappend` Nada" $ do
--            Only (Sum 1) `mappend` Nada `shouldBe` Only(Sum {getSum = 1})
--        it "Only [1] `mappend` Nada" $ do
--            Only [1] `mappend` Nada `shouldBe` Only[1]
--        it "Nada `mappend` Only (Sum 1)" $ do
--            Nada `mappend` Only (Sum 1) `shouldBe` Only(Sum {getSum=1})

