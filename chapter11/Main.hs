module Main where

import Jammin
import Data.List (sortBy, groupBy)

main :: IO ()
main = do 
    putStrLn $ show $ Jam Peach (10::Int)
    putStrLn $ show $ JamJarsxx { fruit = Peach, n = 10 }
    putStrLn $ show $ allJams
    putStrLn $ show $ rowJars $ allJams
    putStrLn $ show $ sumJars $ allJams
    putStrLn $ show $ mostRow $ allJams
    putStrLn $ show $ sortBy  compareKind $ allJams
    putStrLn $ show $ groupJam allJams
