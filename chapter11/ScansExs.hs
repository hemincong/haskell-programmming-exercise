module ScansExs where

fibs = 1 : scanl (+) 1 fibs

fibsN x = fibs !! x

factorial :: [Integer]
factorial = 1 : 1: ff 1 1
        where ff a b = (a + b) : ff b (a +b)

stops = "pbtdkg"
vowels = "aeiou"

-- a)
svs stops vowels = [(s,v,s') | s <- stops, v <- vowels, s' <- stops]

-- b)
svsP stops vowels = [(s,v,s') | s <- stops, v <- vowels, s' <- stops, s == 'p']

-- c)
nouns = ["apple", "banana", "orange"]
verbs = ["turns", "peals", "eats"]
nvn nouns verbs = [(n,v,n') | n <- nouns, v <- verbs, n' <- nouns]

myAnd2 :: [Bool] -> Bool
myAnd2 = foldr (&&) True

myOr2 :: [Bool] -> Bool
myOr2 = foldr (||) False

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f = foldr (\x y -> (f x) || y) False

myElem2 :: (Eq a) => a -> [a] -> Bool
myElem2 a = foldr (\x y -> (x == a) || y) False

myReverse2 :: [a] -> [a]
myReverse2 = foldl (flip (:)) []

myMap2 :: ( a -> b ) -> [a] -> [b]
myMap2 f = foldr (\x y -> f x : y ) []

myFilter2 :: ( a -> Bool ) -> [a] -> [a]
myFilter2 f = foldr (\x y -> if (f x) then (x:y) else y)[]

squish2 :: [[a]] -> [a]
squish2 = foldr (++) []

squishMap2 :: (a -> [b]) -> [a] -> [b]
squishMap2 f = foldr (\x y -> f x ++ y) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap2 id

myMaximumBy ::(a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = foldr pr x xs
                where pr x y
                        | f x y == GT = x 
                        | otherwise = y


myMinimumBy ::(a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = foldr pr x xs
                where pr x y 
                        | f x y == LT = x
                        | otherwise = y
