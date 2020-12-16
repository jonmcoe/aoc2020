module Days.Day14 where

import Data.Bits (clearBit, setBit)
import Days.Common (rightOrDie)
import Text.Parsec
import qualified Data.Map as M

type MaskList = [(Int, Char)]
data BinaryManipulatingInstruction = Mask MaskList | Mem Int Int deriving Show
data MachineState = MachineState {
  entries :: M.Map Int Int,
  mask :: MaskList
}
emptyMachine :: MachineState
emptyMachine = MachineState {entries = M.empty, mask = []}

parseLine :: Parsec String () BinaryManipulatingInstruction
parseLine = try parseMask <|> parseMem
  where
    parseMem = do
      _ <- string "mem["
      addr <- many1 digit
      _ <- string "] = "
      val <- many1 digit
      return $ Mem (read addr) (read val)
    parseMask = do
      _ <- string "mask = "
      s <- many1 (oneOf "X01")
      return $ Mask $ zip [0..] (reverse s)


applyMaskSimple :: MaskList -> Int -> Int
applyMaskSimple masky n = foldl (\acc (i,bitVal) -> alterBit bitVal acc i) n masky
  where
    alterBit '1' = setBit
    alterBit '0' = clearBit
    alterBit _ = const

applyMaskWithFloaters :: MaskList -> Int -> [Int]
applyMaskWithFloaters masky n = foldl makeList [n] masky
  where
    makeList acc (i, bitVal) = concatMap (f bitVal i) acc
    f '1' i' n' = [setBit n' i']
    f '0' _ n' = [n']
    f 'X' i' n' = [setBit n' i', clearBit n' i']
    f  c  _ _ = error $ "bad: "  ++ [c]

runInstruction1 :: MachineState -> BinaryManipulatingInstruction -> MachineState
runInstruction1 mac (Mask m)  = mac {mask = m}
runInstruction1 mac (Mem k v) = mac {entries = M.insert k newValue (entries mac)}
  where newValue = applyMaskSimple (mask mac) v

runInstruction2 :: MachineState -> BinaryManipulatingInstruction -> MachineState
runInstruction2 mac (Mask m)  = mac {mask = m}
runInstruction2 mac (Mem k v) = mac {entries = M.union newEntries (entries mac)}
  where newEntries = M.fromList [(k' , v) | k' <- applyMaskWithFloaters (mask mac) k]

sumOfAllMemoryValuesAtCompletion :: (MachineState -> BinaryManipulatingInstruction -> MachineState) -> String -> Int
sumOfAllMemoryValuesAtCompletion f =
  sum . M.elems . entries . foldl f emptyMachine . map (rightOrDie . parse parseLine "") . lines

day14a :: String -> String
day14a = show . sumOfAllMemoryValuesAtCompletion runInstruction1

day14b :: String -> String
day14b = show . sumOfAllMemoryValuesAtCompletion runInstruction2
