module Days.Day08 where

import Days.Common (rightOrDie)
import Text.Parsec.Char (oneOf, char)
import Text.Parsec
import Data.Char (toUpper)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as S


data Operation = NOP | JMP | ACC deriving (Show, Read, Eq)
type Instruction = (Operation, Int)
data Machine = Machine
  { instructions :: [Instruction]
  , alreadyRan :: S.Set Int
  , position :: Int
  , accumulatorValue :: Int
  } deriving Show

step :: Machine -> Machine
step m = case currentInstruction of
    (NOP, _) -> m {position = position m + 1, alreadyRan = newAlreadyRan}
    (ACC, v) -> m {position = position m + 1, alreadyRan = newAlreadyRan, accumulatorValue = accumulatorValue  m + v}
    (JMP, v) -> m {position = position m + v, alreadyRan = newAlreadyRan}
  where
    newAlreadyRan = S.insert (position m) (alreadyRan m)
    currentInstruction = instructions m !! position m

lineParse :: Parsec String () Instruction
lineParse = do
  op <- choice $ map (try . string) ["nop", "jmp", "acc"]
  _ <- char ' '
  _ <- optional $ char '+'
  val <- many1 (oneOf "-0123456789")
  return (read $ map toUpper op, read val)

parseMachine :: String -> Machine
parseMachine s = Machine {instructions = map rightOrDie insList, alreadyRan = S.empty, position = 0, accumulatorValue = 0}
  where
    insList = map (parse lineParse "") (lines s)

runMachine :: Machine -> Machine
runMachine = until (\ m' -> S.member (position m') (alreadyRan m') || position m' >= length (instructions m')) step

day08a :: String -> String
day08a = show . accumulatorValue . until (\m -> S.member (position m) (alreadyRan m)) step . parseMachine

variations :: Machine -> [Machine]
variations origM = [swapNth origM n | (n,(_,_)) <- filter (\(_,(o,_)) -> o `elem` [JMP, NOP]) (zip [0..] (instructions origM))]
  where
    swapNth m n = let (l, r) = splitAt n (instructions m) in m {instructions = l ++ swap (head r):tail r}
    swap (op, val) = case op of
     JMP -> (NOP, val)
     NOP -> (JMP, val)
     otherOp -> error $ "not swappable: " ++ show otherOp

day08b :: String -> String
day08b = show . accumulatorValue . fromJust .
  find (\m -> position m >= length (instructions m)) .
  map runMachine . variations . parseMachine
