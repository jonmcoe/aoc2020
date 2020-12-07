module Days.Day07 where

import Data.List.Split
import qualified Data.Map as M

type Requirement = [(Int, String)]

myBag :: String
myBag = "hinygoldbag"

endBag :: String
endBag = "nootherbag"

parse :: String -> M.Map String Requirement
parse = M.fromList . map parseLine . lines

parseLine :: String -> (String, Requirement)
parseLine s = (outerBag, map toTuple innerBagList)
  where
    toTuple l = (read [head l] :: Int, tail l)
    innerBagList = if innerBagString == endBag then [] else splitOn "," innerBagString
    innerBagString = halfSplit !! 1
    outerBag = head halfSplit
    halfSplit = splitOn "contain" $ filter (`notElem` " s.") s

ruleAgg :: M.Map String Requirement -> Int -> String -> Requirement
ruleAgg ruleMap factor start = 
  (factor, start) : 
  concat [ruleAgg ruleMap (factor * fst l) (snd l) | l <- M.findWithDefault [] start ruleMap]

day07a :: String -> String
day07a s = show $ length $ filter (elem myBag) $ map (map snd . ruleAgg ruleMap 1) ruleList
  where
    ruleMap = parse s
    ruleList = filter (/= myBag) $ M.keys $ parse s

day07b :: String -> String
day07b s = show $ sum (map fst $ ruleAgg ruleMap 1 myBag) - 1
  where
    ruleMap = parse s
