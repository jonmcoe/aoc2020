module Days.Day10 where

import Data.List (group)
import Data.Sort (sort)

parse :: String -> [Int]
parse = map read . lines

tribonacci :: Int -> Int
tribonacci 1 = 1
tribonacci 2 = 2
tribonacci 3 = 4
tribonacci n = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)

differences :: [Int] -> [Int]
differences = snd . differencesWith
  where differencesWith = foldl (\(mostRecent, acc) incoming -> (incoming, incoming - mostRecent:acc)) (0,[])

day10a :: String -> String
day10a = show . product . map length . group . sort . (3:) . differences . sort. parse

day10b :: String -> String
day10b = show . product . map (tribonacci . length) . filter (\x -> head x == 1) . group . differences . sort . parse
