module Language where
  
import Data.Char (toUpper)
  
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (h:t) = toUpper(h) : t

capitalizeParagraph :: String -> String
capitalizeParagraph s =  xx s True
  where
    xx [] _ = []
    xx ('.':t) isSecHead = '.': xx t True
    xx (' ':t) isSecHead = ' ': xx t isSecHead
    xx (h:t) True = toUpper h : xx t False 
    xx (h:t) False = h: xx t False
