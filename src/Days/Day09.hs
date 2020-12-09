module Days.Day09 where

import Data.List (find, inits, tails)
import Data.Maybe (fromJust)

parse :: String -> [Int]
parse = map (\x -> read x :: Int) . lines

contiguousSubSeqs :: [a] -> [[a]]
contiguousSubSeqs ls = [t | i <- inits ls, t <- tails i, not $ null t]

groupSequences :: Int -> [Int] -> [([Int], Int)]
groupSequences n = map (\l -> let (summers, [target]) = splitAt n l in (summers, target)) .
  filter (\l -> length l == n+1) . contiguousSubSeqs

fulfills :: [Int] -> Int -> Bool
fulfills l t = any (\l' -> sum l' == t) pairs
  where pairs = [[x,y] | x <- init l, y <- tail l, x /= y]

invalid :: [Int] -> Int
invalid = snd . fromJust . find (not . uncurry fulfills) . groupSequences 25

day09a :: String -> String
day09a = show . invalid . parse

day09b :: String -> String
day09b s = show (minimum matchingSeq + maximum matchingSeq)
  where
    matchingSeq = fromJust $ find (\l -> sum l == target) $ contiguousSubSeqs parsed
    target = invalid parsed
    parsed = parse s
