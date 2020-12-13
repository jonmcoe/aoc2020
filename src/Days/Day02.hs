module Days.Day02 where

import Data.List
--import Text.ParserCombinators.ReadP
import Data.List.Split

data Policy =
  Policy
    { leftParam :: Int
    , rightParam :: Int
    , characterOfInterest :: Char
    }
  deriving (Show)

-- TODO: this could use a real parser combinator
parseLine :: String -> (Policy, String)
parseLine s =
  ( Policy {leftParam = read $ head asList, rightParam = read $ asList !! 1, characterOfInterest = head $ asList !! 2}
  , asList !! 3)
  where
    asList = filter (/= "") $ splitOneOf " -:" s
--parseLine :: String -> readP (Policy, String)
--parseLine s = (Policy{minOccur=1,maxOccur=2,characterOfInterest='a'}, "arf")
--parse s = (Policy{minOccur=read lower,maxOccur=read upper,characterOfInterest=coi}, pw)
--  where
--    pw = tail $ dropWhile (/= ' ') afterUpper
--    coi = head $ tail afterUpper
--    (upper, afterUpper) = partition (== ' ') $ tail afterLower
--    (lower, afterLower) = partition (== '-') s

fulfillsPolicyA :: Policy -> String -> Bool
fulfillsPolicyA policy candidatePassword = count >= leftParam policy && count <= rightParam policy
  where
    count = length $ filter (== characterOfInterest policy) candidatePassword

fulfillsPolicyB :: Policy -> String -> Bool
fulfillsPolicyB policy candidatePassword = length candidatePassword >= rightParam policy && aMatches /= bMatches
  where
    aMatches = candidatePassword !! (leftParam policy - 1) == characterOfInterest policy
    bMatches = candidatePassword !! (rightParam policy - 1) == characterOfInterest policy

day02a :: String -> String
day02a = show . length . filter (uncurry fulfillsPolicyA) . map parseLine . lines

day02b :: String -> String
day02b = show . length . filter (uncurry fulfillsPolicyB) . map parseLine . lines