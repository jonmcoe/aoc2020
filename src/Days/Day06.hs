module Days.Day06 where

import Data.List (group)
import Data.List.Split
import Data.Sort
import qualified Data.Set as S

anyone :: [String] -> Int
anyone = length . group . sort . concat

everyone :: [String] -> Int
everyone = length . foldl1 S.intersection . map S.fromList

declarationsAgg :: ([String] -> Int) -> String -> Int
declarationsAgg f = sum . map (f . words . unwords)  . splitOn [""] . lines

day06a :: String -> String
day06a = show . declarationsAgg anyone

day06b :: String -> String
day06b = show . declarationsAgg everyone