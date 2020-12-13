module Days.Day07 where

import Data.Char (digitToInt)
import Data.List.Split
import qualified Data.Map as M

type Requirement = [(Int, String)]

myBag :: String
myBag = "hinygoldbag" -- lol

endBag :: String
endBag = "nootherbag"

parse :: String -> M.Map String Requirement
parse = M.fromList . map parseLine . lines

-- TODO: this could use a real parser combinator
parseLine :: String -> (String, Requirement)
parseLine s = (outerBag, map toTuple innerBagList)
  where
    toTuple l = (digitToInt $ head l, tail l)
    innerBagList = if innerBagString == endBag then [] else splitOn "," innerBagString
    innerBagString = halfSplit !! 1
    outerBag = head halfSplit
    halfSplit = splitOn "contain" $ filter (`notElem` " s.") s -- lol

ruleAgg :: M.Map String Requirement -> Int -> String -> Requirement
ruleAgg ruleMap factor start =
  (factor, start) :
  concat [ruleAgg ruleMap (factor * fst l) (snd l) | l <- M.findWithDefault [] start ruleMap]
-- ALTERNATIVE:  concatMap (\(f,s) -> ruleAgg ruleMap (factor * f) s) (M.findWithDefault [] start ruleMap)

day07a :: String -> String
day07a s = show $ length $ filter (elem myBag) $ map (map snd . ruleAgg ruleMap 1) ruleList
  where
    ruleMap = parse s
    ruleList = filter (/= myBag) $ M.keys $ parse s

day07b :: String -> String
day07b s = show $ sum (map fst $ ruleAgg ruleMap 1 myBag) - 1
  where
    ruleMap = parse s
