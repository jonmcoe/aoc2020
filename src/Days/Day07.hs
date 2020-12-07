module Days.Day07 where

import Data.List.Split
import qualified Data.Map as M

type Requirement = [(Int, String)]

myBag :: String
myBag = "hinygoldbag"

parse :: String -> M.Map String Requirement
parse = M.fromList . map parseLine . lines

parseLine :: String -> (String, Requirement)
parseLine s = (outerBag, map toTuple innerBagList)
  where
    toTuple l = (read [head l] :: Int, tail l)
    innerBagList = splitOn "," (halfSplit !! 1)
    outerBag = head halfSplit
    halfSplit = splitOn "contain" $ filter (`notElem` " s.") s

ruleExpandedContents :: M.Map String Requirement -> String -> [String]
ruleExpandedContents ruleMap start = start : concat [ruleExpandedContents ruleMap l | l <- map snd $ M.findWithDefault [] start ruleMap]

day07a :: String -> String
day07a s = show $ length $ filter (elem myBag) $ map (ruleExpandedContents ruleMap) ruleList
  where
    ruleMap = parse s
    ruleList = filter (/= myBag) $ M.keys $ parse s

day07b :: String -> String
day07b = const "b"