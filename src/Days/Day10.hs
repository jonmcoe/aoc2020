module Days.Day10 where

import Data.List (group)
import Data.Sort (sort)

parse :: String -> [Int]
parse = map read . lines

differences :: [Int] -> [Int]
differences = snd . differencesWith
  where differencesWith = foldl (\(mostRecent, acc) incoming -> (incoming, incoming - mostRecent:acc)) (0,[])

day10a :: String -> String
day10a = show . product . map length . group . sort . (3:) . differences  . sort. parse

day10b :: String -> String
day10b = const "b"