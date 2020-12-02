module Main where

import System.Environment
import Lib

main :: IO ()
main = do
   args <- getArgs
   let
     chosenDay = head args
     (p1, p2) = daysMapping chosenDay
     dataFile = case args of
       _:f:_ ->  f
       _     -> "./data/p" ++ chosenDay
   fullText <- readFile dataFile
   putStrLn $ p1 fullText
   putStrLn $ p2 fullText
