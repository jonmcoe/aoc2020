module Days.Day07 where

import Data.List.Split
import qualified Data.Map as M

myBag :: String
myBag = "hinygoldbag"

parse :: String -> M.Map String [String]
parse = M.fromList . map parseLine . lines

parseLine :: String -> (String, [String])
parseLine s = (head halfSplit, splitOn "," (halfSplit !! 1))
  where halfSplit = splitOn "contain" $ filter (`notElem` " s.0123456789") s

ruleExpandedContents :: M.Map String [String] -> String -> [String]
ruleExpandedContents ruleMap start = start : concat [ruleExpandedContents ruleMap l | l <- M.findWithDefault [] start ruleMap]

day07a :: String -> String
day07a s = show $ length $ filter (elem myBag) $ map (ruleExpandedContents ruleMap) ruleList
  where
    ruleMap = parse s
    ruleList = filter (/= myBag) $ M.keys $ parse s

day07b :: String -> String
day07b = const "b"