module Days.Day01 where

import qualified Data.Set as Set
import Data.List (find, tails)
import Data.Maybe (fromMaybe)

target = 2020

readInt :: String -> Integer
readInt = read

parse = map readInt . lines

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

findSummingGroup :: Int -> [Integer] -> [Integer]
findSummingGroup n l = fromMaybe [] ans
  where
    ans = find (\x -> sum x == target) combos
    combos = combinations n l

--findPair :: [Integer] -> [Integer]
--findPair l = [a, target - a]
--  where
--    a = fromMaybe 0 $ find (`Set.member` asSet) l
--    asSet = Set.fromList $ map (\x -> target - x) l

day01a :: String -> String
day01a = show . product . findSummingGroup 2 . parse

day01b :: String -> String
day01b = show . product . findSummingGroup 3. parse
