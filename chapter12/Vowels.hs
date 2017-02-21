module Vowels where

vowels = "aouie"

filterVowels = filter (\c -> elem c vowels)  

countVowles = length . filterVowels

filterConsonats = filter (\c -> (elem c vowels /= True ))

countConsonats = length . filterConsonats

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if countConsonats s > countVowles s then Just (Word' s) else Nothing

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger (Succ n) = natToInteger n + 1
natToInteger Zero = 0

integerToNat :: Integer -> Maybe Nat
integerToNat n  
        | n >= 0 = Just (iToNat n)
        | otherwise = Nothing

iToNat :: Integer -> Nat
iToNat 0 = Zero
iToNat n = Succ (iToNat (n-1))


main :: IO ()
main = do
    putStrLn $ show $ countVowles "Hello, Haskell!"
    putStrLn $ show $ countVowles "the cow"
    putStrLn $ show $ countVowles "Mikolajczak"
    putStrLn $ show $ countConsonats $ "the cow"
    putStrLn $ show $ countConsonats "Mikolajczak"
    putStrLn $ show $ mkWord "Mikolajczak"
    putStrLn $ show $ mkWord "oaaap"
    putStrLn $ show $ mkWord "the cow"
    putStrLn $ show $ natToInteger Zero
    putStrLn $ show $ natToInteger (Succ Zero)
    putStrLn $ show $ natToInteger (Succ (Succ Zero))
    putStrLn $ show $ integerToNat 0
    putStrLn $ show $ integerToNat 1
    putStrLn $ show $ integerToNat 2
    putStrLn $ show $ integerToNat (-1)
