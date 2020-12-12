module Days.Day12 where

type Position = (Int, Int)
data Bearing = Northy | Southy | Easty | Westy deriving (Show, Eq)
data NavInstruction =
  North Int | South Int | East Int | West Int |
  Leftwards Int | Rightwards Int | Forward Int deriving (Show, Eq)

parseNavInstruction :: String -> NavInstruction
parseNavInstruction (x:xs)
  | x == 'N' = North (read xs)
  | x == 'S' = South (read xs)
  | x == 'E' = East (read xs)
  | x == 'W' = West (read xs)
  | x == 'L' = Leftwards (read xs)
  | x == 'R' = Rightwards (read xs)
  | x == 'F' = Forward (read xs)
  | otherwise = error "bad first character"
parseNavInstruction _ = error "bad length"

parseInstructions :: String -> [NavInstruction]
parseInstructions = map parseNavInstruction . lines

rotate :: Bearing -> NavInstruction -> Bearing
rotate bearing (Leftwards 90)
  | bearing == Northy = Westy
  | bearing == Southy = Easty
  | bearing == Easty = Northy
  | bearing == Westy = Southy
rotate bearing (Leftwards other) = rotate (rotate bearing (Leftwards 90)) (Leftwards (other - 90))
rotate bearing (Rightwards other) = rotate bearing (Leftwards (360 - other))

executeInstruction :: (Position, Bearing) -> NavInstruction -> (Position, Bearing)
executeInstruction ((x,y), bearing) (North mag) = ((x, y + mag), bearing)
executeInstruction ((x,y), bearing) (South mag) = ((x, y - mag), bearing)
executeInstruction ((x,y), bearing) (East mag) = ((x + mag, y), bearing)
executeInstruction ((x,y), bearing) (West mag) = ((x - mag, y), bearing)
executeInstruction ((x,y), bearing) (Forward mag)
  | bearing == Northy = ((x, y + mag), bearing)
  | bearing == Southy = ((x, y - mag), bearing)
  | bearing == Easty = ((x + mag, y), bearing)
  | bearing == Westy = ((x - mag, y), bearing)
executeInstruction ((x,y), bearing) turn = ((x,y), rotate bearing turn)

day12a :: String -> String
day12a = show . (\((x,y),_) -> abs x + abs y) . foldl executeInstruction ((0,0), Easty) . parseInstructions

day12b :: String -> String
day12b = const "b"
