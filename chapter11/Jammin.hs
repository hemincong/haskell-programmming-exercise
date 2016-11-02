module Jammin where

import Data.List (sortBy, groupBy)

data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Show, Ord)

data JamJars = Jam Fruit Int deriving (Eq, Show)

data JamJarsxx = JamJarsxx { fruit :: Fruit, n :: Int} deriving (Eq, Show, Ord)

row1 = JamJarsxx { fruit = Peach, n = 10 }
row2 = JamJarsxx { fruit = Peach, n = 20 }
row3 = JamJarsxx { fruit = Plum, n = 20 }
row4 = JamJarsxx Apple 90
row5 = JamJarsxx Blackberry 10
row6 = JamJarsxx Plum 20

allJams = [ row1, row2, row3, row4, row5, row6]

rowJars :: [JamJarsxx] -> [Int]
rowJars = map n 

sumJars :: [JamJarsxx] -> Int
sumJars = sum . rowJars 

mostRow :: [JamJarsxx] -> JamJarsxx
mostRow [] = undefined
mostRow (x:xs) = foldr f x xs
    where f x y 
            | n x < n y = y
            | otherwise = x
                
compareKind :: JamJarsxx -> JamJarsxx -> Ordering
compareKind x y = compare (fruit x) (fruit y)

groupJam :: [JamJarsxx] -> [[JamJarsxx]]
groupJam = groupBy (\x y -> fruit x == fruit y) . sortBy compareKind


