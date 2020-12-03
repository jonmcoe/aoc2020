module Days.Day03 where

tree = '#'

numTreesHit :: Int -> Int -> [String] -> Int
numTreesHit xStep yStep g = length $ filter (== tree) $ map (\(i, r) -> r!!(i*xStep `mod` (length r))) numbered
  where numbered = zip [0..] $ map snd $ filter (\(i, _) -> i `mod` yStep == 0) (zip [0..] g)

day03a :: String -> String
day03a = show . numTreesHit 3 1 . lines

day03b :: String -> String
day03b s = show $ product [numTreesHit 1 1 grid, numTreesHit 3 1 grid, numTreesHit 5 1 grid, numTreesHit 7 1 grid, numTreesHit 1 2 grid]
  where grid = lines s
