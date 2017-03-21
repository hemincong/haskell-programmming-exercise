module UsingQuickCheck where

import Data.List (sort)
import Data.Char (toUpper)

import Test.QuickCheck
import Test.QuickCheck.Modifiers (NonZero)
import Test.QuickCheck.Function (Fun(..))

half x = x / 2

halfIdentity = (*2) . half 

propHalf x = halfIdentity x == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

porpSort :: [Int] -> Bool
porpSort xs
    | listOrdered xs = xs == (sort xs)
    | otherwise = xs /= (sort xs)

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x


multiAssociative :: Int -> Int -> Int -> Bool
multiAssociative x y z = x * (y * z) == (x * y) * z

multiCommutative :: Int -> Int -> Bool
multiCommutative x y = x * y == y * x

propQuotRem :: NonZero Int -> NonZero Int -> Bool
propQuotRem (NonZero x) (NonZero y) = (quot x y) * y + (rem x y ) == x

powerAssociative :: Int -> Int -> Int -> Bool
powerAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

powerCommutative :: Int -> Int -> Bool
powerCommutative x y  = x ^ y ==  y ^ x

propReverse :: [Int] -> Bool
propReverse xs = (reverse .reverse) xs == id xs

applyProp :: Fun Int Int -> Int -> Bool
applyProp  (Fun _ f) a = (f $ a) == (f a)

composeProp :: Fun Char Double -> Fun Int Char -> Int -> Bool
composeProp (Fun _ f) (Fun _ g) x = (f . g) x == f (g x)

inConcatProp :: String -> String -> Bool
inConcatProp xs ys = foldr (:) xs ys == (++) xs ys

concatProp :: [String] -> Bool
concatProp xs = foldr (++) [] xs == concat xs

lengthTakeProp :: Int -> String -> Bool
lengthTakeProp n xs = length (take n xs) == n

readShowProp :: Int -> Bool
readShowProp x = (read (show x)) == x

square x = x * x

squareIdentity = square . sqrt 

squareProp x = squareIdentity x == x

twice f = f . f

fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (h:t) = toUpper(h) : t

capitalizeWordProp :: String -> Bool
capitalizeWordProp x = (capitalizeWord x == twice capitalizeWord  x) && (capitalizeWord x == fourTimes capitalizeWord x)

sortProp :: [Int] -> Bool
sortProp xs = (sort xs == twice sort xs) && (sort xs == fourTimes sort xs)
