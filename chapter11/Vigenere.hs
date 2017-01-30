module Vigenere where
  
import Data.Char
  
encode:: Char -> Int
encode c = ord c - ord 'a'

decode:: Int -> Char
decode n = chr (ord 'a' + n)

shift :: (Int -> Int -> Int) -> Char ->  Int-> Char
shift f c n = decode $ mod (f (encode c) n) 26 

rightShift :: Char -> Int -> Char
rightShift = shift (+)

leftShift :: Char -> Int-> Char
leftShift = shift (-)

vigenere :: String -> String -> String
vigenere s2 = zipWith (\a b -> rightShift a (encode b)) (concat $ repeat s2)


unVigenere :: String -> String -> String
unVigenere s2 = zipWith (\a b -> leftShift b (encode a)) (concat $ repeat s2)
