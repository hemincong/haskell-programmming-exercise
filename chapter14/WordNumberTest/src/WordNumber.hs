module WordNumber
    ( wordNumber
    , digitToWord
    , digits
    ) where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord _ = "nine"

digits :: Int -> [ Int ]
digits n  
    | n < 10 = [n]
    | otherwise = digits (n `div` 10) ++ [(n `mod` 10)]

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
