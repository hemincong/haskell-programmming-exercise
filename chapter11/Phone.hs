module Phone where
  
import Data.List
import Data.Char
import Data.Ord

  
convo :: [String]
convo = [
  "Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I an pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"
  ]
  
type Digit = Char 

type Presses = Int 

data Button = Button Digit String deriving Show

data DaPhone = DaPhone [Button] deriving Show

maximumBy' :: Ord a => (a -> a -> Ordering) -> [(t, a)] -> (t, a)
maximumBy' _ [] = error "maximum of empty list"
maximumBy' f (x:xs) = findMax x xs
      where findMax currMax [] = currMax
            findMax (t, a) (p:ps)
                | f a ( snd p ) == LT = findMax p ps
                | otherwise = findMax (t, a) ps
                
keyBorad :: DaPhone
keyBorad = DaPhone [
            Button '1' "1",     Button '2' "2ABC",  Button '3' "3DEF",
            Button '4' "4GHI",  Button '5' "5JKL",  Button '6' "6MNO",
            Button '7' "7PQRS", Button '8' "8TUV",  Button '9' "9WXYZ",
            Button '*' "*^",    Button '0' "0+ ",   Button '#' "#.,"
           ]
           
checkButton :: Button -> Char -> [(Digit, Presses)]
checkButton (Button c s) ch 
            | isLower ch = case elemIndex (toUpper ch) s of 
                                Nothing -> []
                                Just n -> [(c, n+1)] 
            | otherwise = case elemIndex ch s of
                                Nothing -> []
                                Just n -> [(c, n+1)]
                                
vailChars :: String
vailChars = ['a'..'z']++['A'..'Z']++"*^+#.,"

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone keys) ch = concatMap (flip checkButton ch) keys

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = concatMap . reverseTaps

cellProgDead :: DaPhone -> [String] -> [(Digit, Presses)]
cellProgDead = concatMap . cellPhonesDead

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) b -> p + b) 0

letterPreses :: Char -> String -> Int
letterPreses c s = length $ filter (\x -> x == c) s

allLetterPresses :: String -> [(Char, Int)]
allLetterPresses s = foldr (\c acc -> (c, letterPreses c s) : acc) [] vailChars

mostPopularLetter :: String -> Char
mostPopularLetter = fst . maximumBy' compare . allLetterPresses 

coolestLtr :: [String] -> Char
coolestLtr = fst. minimumBy compare . allLetterPresses . filter (flip elem vailChars) . concat . map (\s -> s ++ " ")

coolestWord :: [String] -> String
coolestWord = head . minimumBy (\x y -> compare (length x) (length y)) . group . sort . words . concat . map (\s -> s ++ " ")
