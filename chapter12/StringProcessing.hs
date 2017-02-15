module StringProcessing where
  
import Data.List (intercalate)
  
notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

replaceThe :: String -> String
replaceThe s = intercalate " " $ map theToA $ map notThe $ (words s)
  where 
    theToA (Just s) = s
    theToA Nothing = ""


