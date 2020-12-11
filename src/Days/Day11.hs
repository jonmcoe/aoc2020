module Days.Day11 where

nextChar :: [[Char]] -> Int -> Int -> Char
nextChar g x y = case current of
  'L' -> if '#' `elem` surroundings then 'L' else '#'
  '#' -> if length (filter (== '#') surroundings) >= 4 then 'L' else '#'
  c -> c
  where
    current = g!!y!!x
    surroundings = [g!!y'!!x' | y' <- [topY..bottomY], x' <- [leftX..rightX], not (y == y' && x == x')]
    leftX = max (x-1) 0
    rightX = min (x+1) (length (head g) - 1)
    topY = max (y-1) 0
    bottomY = min (y+1) (length g - 1)

nextGrid :: [[Char]] -> [[Char]]
nextGrid g = [
  [nextChar g x y | x <- [0..(length (head g) - 1)]]
  | y <- [0..length g - 1]]

day11a :: String -> String
day11a = show . sum . map (length . filter (== '#')) . until (\g -> nextGrid g == g) nextGrid . lines

day11b :: String -> String
day11b = const "b"
