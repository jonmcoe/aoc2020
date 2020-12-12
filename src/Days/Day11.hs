module Days.Day11 where

import Data.List (find)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

type Grid = M.Map (Int, Int) Char
type AdjacencyFunction = Grid -> (Int, Int) -> [Char]

directions :: [(Int, Int)]
directions = [(dx,dy) | dy <- [-1..1], dx <- [-1..1], not (dx == 0 && dy == 0)]

nextChar :: AdjacencyFunction -> Int -> Grid -> (Int, Int) -> Char -> Char
nextChar adjacencyFunc threshold g t current = case current of
  'L' -> if '#' `elem` surroundings then 'L' else '#'
  '#' -> if length (filter (== '#') surroundings) >= threshold then 'L' else '#'
  c -> c
  where
    surroundings = adjacencyFunc g t

nextGrid :: AdjacencyFunction -> Int -> Grid -> Grid
nextGrid adjacencyFunc threshold g = M.mapWithKey nextCharPartial g
  where nextCharPartial = nextChar adjacencyFunc threshold g

makeMap :: (Integral a) => [[b]] -> M.Map (a, a) b
makeMap m = M.fromList $ concat [[((x, y), c) | (x, c) <- zip [0 ..] r] | (y, r) <- zip [0 ..] m]

untilRepeated :: Eq t => (t -> t) -> t -> t
untilRepeated f a
  | nextVal == a = a
  | otherwise = untilRepeated f nextVal
  where nextVal = f a

runGridToCompletion :: AdjacencyFunction -> Int -> String -> String
runGridToCompletion f threshold =
  show . length . filter (== '#') . M.elems .
  untilRepeated nextGridPartial .
  makeMap . lines
  where nextGridPartial = nextGrid f threshold

simpleAdjacency :: AdjacencyFunction
simpleAdjacency g (x,y) = mapMaybe (\(dx, dy) -> g M.!? (x + dx, y + dy)) directions

day11a :: String -> String
day11a = runGridToCompletion simpleAdjacency 4

visibilityAdjacency :: AdjacencyFunction
visibilityAdjacency g t = mapMaybe firstInDirection directions
  where
    firstInDirection (dx, dy) = find (/= '.') $ map (g M.!) $ takeWhile (`M.member` g) $
      drop 1 $ iterate (\(y', x') -> (y' + dy, x' + dx)) t

day11b :: String -> String
day11b = runGridToCompletion visibilityAdjacency 5
