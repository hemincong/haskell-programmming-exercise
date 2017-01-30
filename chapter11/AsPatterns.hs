module AsPatterns where
  
import Data.Char
  
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
    print a
    return t
    
doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp (x:xs) = x:x:xs

isSubsequnenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequnenceOf [] _ = True
isSubsequnenceOf _ [] = False
isSubsequnenceOf (x:xs) (y:ys) = (x == y && isSubsequnenceOf xs ys) || isSubsequnenceOf (x:xs) ys


capitalizeWords :: String -> [(String, String)]
capitalizeWords  = xx . words 
            where 
              xx [] = []
              xx ((h:t):ws) = ((h:t), (toUpper h:t)) : xx ws