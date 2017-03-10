module Main where

import Morse

import Control.Monad (forever, when)
import Data.List(intercalate)
import Data.Traversable(traverse)
import Morse(stringToMorse, morseToChar)
import System.Environment(getArgs)
import System.Exit(exitFailure, exitSuccess)
import System.IO(hGetLine, hIsEOF, stdin)

convertToMorse :: IO()
convertToMorse = forever $ do
    weAreDone <- hIsEOF stdin
    when weAreDone exitSuccess

    line <- hGetLine stdin
    convertLine line
    where convertLine line = do
            let s = stringToMorse line
            case s of 
                (Just ms) -> putStrLn (intercalate " " ms)
                Nothing -> putStrLn "error"

convertFromMorse :: IO()
convertFromMorse = forever $ do
    weAreDone <- hIsEOF stdin
    when weAreDone exitSuccess

    line <- hGetLine stdin
    convertLine line 

    where convertLine line = do
           let decode = traverse morseToChar (words line)
           case decode of 
                (Just s) -> putStrLn s
                Nothing -> putStrLn "error"

main :: IO()
main = do
    mode <- getArgs
    case mode of
        [arg] -> 
            case arg of
                "from" -> convertFromMorse
                "to" -> convertToMorse
                _ -> argError
        _ -> argError

    where argError = do
            putStrLn "Please specify the first argumen as being 'from' or 'to' morse,\
                    \ such as: morse to"
            exitFailure

