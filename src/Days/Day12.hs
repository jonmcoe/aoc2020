module Days.Day12 where

type Position = (Int, Int)
type CardinalFunc = (Position, Position) -> Char -> Int -> (Position, Position)

rotateLeft :: Position -> Int -> Position
rotateLeft (x, y) 90 = (-1 * y, x)
rotateLeft t x
  | x `mod` 90 == 0 = rotateLeft (rotateLeft t 90) (x - 90)
  | otherwise = error $ "unsupported turning degrees" ++ show x

addToPosition :: Position -> Char -> Int -> Position
addToPosition (x,y) inst mag
  | inst == 'N' = (x, y + mag)
  | inst == 'S' = (x, y - mag)
  | inst == 'E' = (x + mag, y)
  | inst == 'W' = (x - mag, y)
  | otherwise = error $ "bad instruction" ++ [inst]

executeInstruction :: CardinalFunc -> (Position, Position) -> String -> (Position, Position)
executeInstruction cardinalFunc ((x,y), (dx, dy)) (inst:magStr)
  | inst == 'F' = ((x + dx * mag, y + dy * mag), (dx, dy))
  | inst == 'L' = ((x, y), rotateLeft (dx, dy) mag)
  | inst == 'R' = ((x, y), rotateLeft (dx, dy) (360 - mag))
  | otherwise = cardinalFunc ((x, y), (dx, dy)) inst mag
  where mag = read magStr
executeInstruction _ _ s = error $ "could not parse instruction: " ++ s

manhattanDistanceFromOriginToEndingPosition :: CardinalFunc -> Position -> String -> Int
manhattanDistanceFromOriginToEndingPosition cardinalFunc initialWaypoint =
  (\((x,y),_) -> abs x + abs y) . foldl (executeInstruction cardinalFunc) ((0,0), initialWaypoint) . lines

day12a :: String -> String
day12a = show . manhattanDistanceFromOriginToEndingPosition addToBoatPosition (1, 0)
  where addToBoatPosition (t, dt) inst mag = (addToPosition t inst mag, dt)

day12b :: String -> String
day12b = show . manhattanDistanceFromOriginToEndingPosition addToWaypointPosition (10, 1)
  where addToWaypointPosition (t, dt) inst mag = (t, addToPosition dt inst mag)
