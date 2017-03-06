module Palindrome where
  
import Control.Monad
import System.Exit (exitSuccess)
import Data.Char(toLower)

palindrome :: IO()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
      return()
      
isPalindrome :: String -> Bool
isPalindrome s = 
  let message = filter (`notElem` ".?!-;\'\"") $ map toLower (concat (words s)) in
    message == reverse message