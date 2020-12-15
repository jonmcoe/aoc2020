module Days.Day14 where

import Data.Bits (clearBit, setBit)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Days.Common (rightOrDie)
import Text.Parsec
import qualified Data.Map as M

type MaskList = [(Int, Bool)]
data BinaryManipulatingInstruction = Mask MaskList | Mem Int Int deriving Show
data MachineState = MachineState {
  entries :: M.Map Int Int,
  mask :: MaskList
}
emptyMachine :: MachineState
emptyMachine = MachineState {entries = M.empty, mask = []}

parseMask :: Parsec String () BinaryManipulatingInstruction
parseMask = do
  _ <- string "mask = "
  s <- many (oneOf "X01")
  let
    f i c = case c of
      'X' -> Nothing
      '0' -> Just (i, False)
      '1' -> Just (i, True)
      _ -> error "bad masky char"
  return $ Mask $ catMaybes $ zipWith f [0..] (reverse s)

parseMem :: Parsec String () BinaryManipulatingInstruction
parseMem = do
  _ <- string "mem["
  addr <- many digit
  _ <- string "] = "
  val <- many digit
  return $ Mem (read addr) (read val)

parseLine :: [Char] -> BinaryManipulatingInstruction
parseLine l
  | "mask" `isPrefixOf` l = rightOrDie $ parse parseMask "" l
  | "mem" `isPrefixOf` l = rightOrDie $ parse parseMem "" l
  | otherwise = error $ "bad line: " ++ l

applyMask :: MaskList -> Int -> Int
applyMask masky n = foldl (\acc (i,bitVal) -> alterBit bitVal acc i) n masky
  where
    alterBit True = setBit
    alterBit False = clearBit

--toBinary :: Int -> [Int] -> [Int]
--toBinary 0 acc = acc
--toBinary n acc = toBinary (n `div` 2) ((n `mod` 2):acc)

runInstruction1 :: MachineState -> BinaryManipulatingInstruction -> MachineState
runInstruction1 mac (Mask m)  = mac {mask = m}
runInstruction1 mac (Mem k v) = mac {entries = M.insert k newValue (entries mac)}
  where newValue = applyMask (mask mac) v

sumOfAllMemoryValuesAtCompletion :: (MachineState -> BinaryManipulatingInstruction -> MachineState) -> String -> Int
sumOfAllMemoryValuesAtCompletion f = sum . M.elems . entries . foldl f emptyMachine . map parseLine . lines

day14a :: String -> String
day14a = show . sumOfAllMemoryValuesAtCompletion runInstruction1

day14b :: String -> String
day14b = show . sumOfAllMemoryValuesAtCompletion runInstruction1