module Days.Day15 where

import Data.List (find)
import Data.List.Split (splitOn)

appendNextEntry :: [Int] -> [Int]
appendNextEntry l = l ++ [length l - indexOfLastOccurrence]
  where
    indexOfLastOccurrence = case lastOccurrence of
      Nothing -> length l
      Just (i, _) -> i + 1
    lastOccurrence = find (\(_, n) -> n == last l) $ reverse $ zip [0..] (take (length l - 1) l)
--
--nextEntry :: [Int] -> Int
--    indexOfLastOccurrence = case lastOccurrence of
--      Nothing -> length l
--      Just (i, _) -> i + 1
--    lastOccurrence = find (\(_, n) -> n == last l) $ reverse $ zip [0..] (take (length l - 1) l)

parse :: String -> [Int]
parse = map read . splitOn ","

day15a :: String -> String
day15a = show . (!! 2019) . (!! 2020) . iterate appendNextEntry . parse

day15b :: String -> String
--day15b = show . (!! 100) . iterate appendNextEntry . parse
day15b = show . (!! (30000000 - 1)) . (!! 30000000) . iterate appendNextEntry . parse -- brute