module Days.Day13 where

import Data.Maybe (fromJust, mapMaybe)
import Data.List (find, minimumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Text.Read (readMaybe)

day13a :: String -> String
day13a s = show $ bestBus * (bestBus - waitTime bestBus)
  where
    bestBus = minimumBy (comparing waitTime) buses
    waitTime busId = busId - (arrival `mod` busId)
    buses = mapMaybe (\x -> readMaybe x :: Maybe Int) $ splitOn "," busesStr
    arrival = read arrivalStr :: Int
    [arrivalStr, busesStr] = lines s

acceptable :: Integral a => a -> (a, a) -> Bool
acceptable start (i, busId) = (i + start) `mod` busId == 0

-- cheats: ran w/ busesWithIndices subsets take 1, take 2, etc iteratively to get starting point
--         and expand those factors on the list comp's delta
--         they did us a favor with prime numbers so no need to find GCD
-- TODO: automate the above process (CRT?) or do something even better after pencil and paper work
day13b :: String -> String
day13b s = show $ fromJust $ find (\t -> all (acceptable t) busesWithIndices) [95940001904734,95940001904734+13*41*641*19..]
  where
    busesWithIndices = mapMaybe unpackJustOrNothing $ zip [0..] buses
    buses = map (\x -> readMaybe x :: Maybe Int) $ splitOn "," busesStr
    unpackJustOrNothing (i, Just x) = Just (i, x)
    unpackJustOrNothing (_, Nothing) = Nothing
    [_, busesStr] = lines s