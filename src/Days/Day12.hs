module Days.Day12 where

type Position = (Int, Int)
type CardinalFunc = (Position, Position) -> Char -> Int -> (Position, Position)

rotateLeft :: Position -> Int -> Position
rotateLeft (x, y) 90 = (-1 * y, x)
rotateLeft t x
  | x `mod` 90 == 0 = rotateLeft (rotateLeft t 90) (x - 90)
  | otherwise = error "unsupported turning degrees"

addToBoatPosition :: CardinalFunc
addToBoatPosition ((x, y), dt) inst mag
  | inst == 'N' = ((x, y + mag), dt)
  | inst == 'S' = ((x, y - mag), dt)
  | inst == 'E' = ((x + mag, y), dt)
  | inst == 'W' = ((x - mag, y), dt)
  | otherwise = error "bad instruction"

addToWaypointPosition :: CardinalFunc
addToWaypointPosition (t, (dx, dy)) inst mag
  | inst == 'N' = (t, (dx, dy + mag))
  | inst == 'S' = (t, (dx, dy - mag))
  | inst == 'E' = (t, (dx + mag, dy))
  | inst == 'W' = (t, (dx - mag, dy))
  | otherwise = error "bad instruction"

executeInstruction :: CardinalFunc -> (Position, Position) -> String -> (Position, Position)
executeInstruction cardinalFunc ((x,y), (dx, dy)) (inst:magStr)
  | inst == 'F' = ((x + dx * mag, y + dy * mag), (dx, dy))
  | inst == 'L' = ((x, y), rotateLeft (dx, dy) mag)
  | inst == 'R' = ((x, y), rotateLeft (dx, dy) (360 - mag))
  | otherwise = cardinalFunc ((x, y), (dx, dy)) inst mag
  where mag = read magStr
executeInstruction _ _ s = error $ "could not parse instruction: " ++ s

day12a :: String -> String
day12a = show . (\((x,y),_) -> abs x + abs y) . foldl (executeInstruction addToBoatPosition) ((0,0), (1,0)) . lines

day12b :: String -> String
day12b = show . (\((x,y),_) -> abs x + abs y) . foldl (executeInstruction addToWaypointPosition) ((0,0), (10, 1)) . lines
