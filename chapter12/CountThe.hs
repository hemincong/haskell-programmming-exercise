module CountThe where

vowels = "aeiou"

startWithVowel :: String -> Maybe String
startWithVowel s = if ((elem ( head $ s) vowels) /= True)  then Just s else Nothing

xx' [] = 0
xx' (x:xs) = case startWithVowel x of
             Nothing -> 0
             (Just s) -> if s == "the" then ((xx' xs) + 1) else (xx' xs)
             
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel ws = if Nothing `elem` map startWithVowel (words $ ws) 
                            then xx' $ words ws
                            else 0