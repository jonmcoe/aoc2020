module Days.Day03 where

tree = '#'

numTreesHit :: Int -> Int -> [String] -> Int
numTreesHit xStep yStep g = length $ filter (== tree) $ map (\(i, r) -> r!!(i*xStep `mod` (length r))) numbered
  where numbered = zip [0..] $ map snd $ filter (\(i, _) -> i `mod` yStep == 0) (zip [0..] g)

day03a :: String -> String
day03a = show . numTreesHit 3 1 . lines

day03b :: String -> String
day03b s = show $ product $ map (\(x, y) -> numTreesHit x y (lines s)) [(1,1),(3,1),(5,1),(7,1),(1,2)]