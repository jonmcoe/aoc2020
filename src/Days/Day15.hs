module Days.Day15 where

import Data.List.Split (splitOn)
import qualified Data.Map as M

next :: (Int, M.Map Int Int, Int) -> (Int, M.Map Int Int, Int)
next (i, m, incomingVal) = (i+1, M.insert incomingVal i m, outgoingVal)
  where
    outgoingVal = case m M.!? incomingVal of
      Nothing -> 0
      Just x -> i - x

parse :: String -> [Int]
parse = map read . splitOn ","

getNthEntry :: Int -> String -> Int
getNthEntry n = 
  (\(_, _, x) -> x) . until (\(i, _, _) -> i == n) next . 
  (\l -> (length l, M.fromList (zip (init l) [1..]), last l)) . parse

day15a :: String -> String
day15a = show . getNthEntry 2020

day15b :: String -> String
day15b = show . getNthEntry 30000000
