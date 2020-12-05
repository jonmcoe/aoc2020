module Days.Day05 where

import           Data.List  (find)
import           Data.Maybe (fromJust)
import qualified Data.Set   as S

binaryConvert :: Char -> String -> Int
binaryConvert oneChar inString =
  foldl (\acc (p,i) -> acc + if i == oneChar then 2^p else 0) 0 zipped
  where
    zipped = zip [length inString - 1, length inString-2..0] inString

seatId :: String -> Int
seatId s = row * 8 + col
  where
    col = binaryConvert 'R' colString
    row = binaryConvert 'B' rowString
    (rowString, colString) = splitAt 7 s

findGap :: [Int] -> Int
findGap l = 1 + fromJust (find (\n -> S.member (n + 2) asSet && not (S.member (n + 1) asSet)) l)
  where asSet = S.fromList l


day05a :: String -> String
day05a = show . maximum . map seatId . lines

day05b :: String -> String
day05b = show . findGap . map seatId . lines
