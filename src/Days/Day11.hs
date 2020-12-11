module Days.Day11 where

import Data.List (find)
import Data.Maybe (mapMaybe)

type AdjacencyFunction = [[Char]] -> Int -> Int -> [Char]

nextChar :: AdjacencyFunction -> Int -> [[Char]] -> Int -> Int -> Char
nextChar adjacencyFunc threshold g x y = case current of
  'L' -> if '#' `elem` surroundings then 'L' else '#'
  '#' -> if length (filter (== '#') surroundings) >= threshold then 'L' else '#'
  c -> c
  where
    current = g!!y!!x
    surroundings = adjacencyFunc g y x

nextGrid :: AdjacencyFunction -> Int -> [[Char]] -> [[Char]]
nextGrid adjacencyFunc threshold g = [
  [nextCharPartial g x y | x <- [0..(length (head g) - 1)]]
  | y <- [0..length g - 1]]
  where nextCharPartial = nextChar adjacencyFunc threshold

runGridToCompletion :: AdjacencyFunction -> Int -> String -> String
runGridToCompletion f threshold = show . sum . map (length . filter (== '#')) . until (\g -> nextGridPartial g == g) nextGridPartial . lines
  where nextGridPartial = nextGrid f threshold

simpleAdjacency :: AdjacencyFunction
simpleAdjacency g y x = [g!!y'!!x' | y' <- [topY..bottomY], x' <- [leftX..rightX], not (y == y' && x == x')]
  where
    leftX = max (x-1) 0
    rightX = min (x+1) (length (head g) - 1)
    topY = max (y-1) 0
    bottomY = min (y+1) (length g - 1)

day11a :: String -> String
day11a = runGridToCompletion simpleAdjacency 4

visibilityAdjacency :: AdjacencyFunction
visibilityAdjacency g y x = mapMaybe firstInDirection directions
  where
    firstInDirection (dx, dy) = find (/= '.') $ map (\(y', x') -> g!!y'!!x') $
      takeWhile inBounds $ drop 1 $ iterate (\(y', x') -> (y' + dy, x' + dx)) (y,x)
    inBounds (y', x') = y' >= 0 && x' >= 0 && y' < length g && x' < length (head g)
    directions = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]


day11b :: String -> String
day11b = runGridToCompletion visibilityAdjacency 5
