module Hangman where

import Control.Monad(forever)

import Data.Maybe(isJust)
import Data.List(intersperse)
import System.Exit(exitSuccess)
import System.Random(randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)
    
minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameWords aw)
  where gameWords w = 
          let l = length (w::String)
          in l > minWordLength && l < maxWordLength
          
randomWord :: WordList -> IO String
randomWord wl = do
          randomIndex <- randomRIO(0, length wl - 1)
          return $ wl !! randomIndex
          
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = 
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed
    
freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (fmap (const Nothing) str) ""

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = c `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = c `elem` g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar previouslyGuessed) c =
  Puzzle word newFilledInSoFar (c:previouslyGuessed)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar
          
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess is: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character; pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word; filling it in..."
      return $ fillInCharacter puzzle guess
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again!"
      return $ fillInCharacter puzzle guess
                                                          
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledIn guesses) =
  if wrongGuesses > 7 then do
    putStrLn "You lose!"
    putStrLn $ "The word was " ++ wordToGuess
    exitSuccess
  else do
    putStrLn $ "Wrong guesses: " ++ wrongGuessCountString
    return ()
  where
    wrongGuesses = length guesses - (length . filter isJust $ filledIn)
    wrongGuessCountString = show wrongGuesses

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"