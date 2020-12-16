module Days.Day15 where

import Data.List.Split (splitOn)
import qualified Data.IntMap as M

next :: (Int, M.IntMap Int, Int) -> (Int, M.IntMap Int, Int)
next (i, m, incomingVal) = (i+1, M.insert incomingVal i m, outgoingVal)
  where
    outgoingVal = case m M.!? incomingVal of
      Nothing -> 0
      Just x -> i - x

getNthEntry :: Int -> String -> Int
getNthEntry n = 
  (\(_, _, x) -> x) . until (\(i, _, _) -> i == n) next . 
  (\l -> (length l, M.fromList (zip (init l) [1..]), last l)) . parse
  where parse = map read . splitOn ","

day15a :: String -> String
day15a = show . getNthEntry 2020

day15b :: String -> String
day15b = show . getNthEntry 30000000
