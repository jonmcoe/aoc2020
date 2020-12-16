module Days.Day16 where

import Days.Common (rightOrDie)
import Data.Maybe (fromJust)
import Text.Parsec
import qualified Data.Map as M
import qualified Data.Set as S

type RulesMap = M.Map String (S.Set Int)
data FullInput =
  FullInput
  { rules :: RulesMap
  , myTicket :: [Int]
  , nearbyTickets :: [[Int]]
  } deriving Show

parseAll :: Parsec String () FullInput
parseAll = do
  rulesList <- parseRule `sepEndBy1` endOfLine
  _ <- skipMany1 endOfLine
  _ <- string "your ticket:\n"
  myTicketIn <- parseIntList
  skipMany1 endOfLine
  _ <- string "nearby tickets:\n"
  nearbyTicketsIn <- parseIntList `sepEndBy1` endOfLine
  return FullInput {
    rules = M.fromList rulesList,
    myTicket=myTicketIn,
    nearbyTickets=nearbyTicketsIn}
  where
    parseIntList = do
      l <- many1 digit `sepBy1` char ','
      return $ map read l

parseRule :: Parsec String () (String, S.Set Int)
parseRule = do
  field <- many1 (choice [alphaNum, char ' '])
  _ <- string ": "
  ranges <- parseRange `sepBy1` string " or "
  let
    rangeSets = [S.fromList [low..high] | (low, high) <- ranges]
  return (field, S.unions rangeSets)
  where
    parseRange = do
      low <- many1 digit
      _ <- char '-'
      high <- many1 digit
      return (read low :: Int, read high :: Int)

day16a :: String -> String
day16a s = show $ sum invalidFields
  where
    invalidFields = concatMap (filter (\i -> not (S.member i fullAcceptable))) (nearbyTickets fullInput)
    fullAcceptable = S.unions $ M.elems $ rules fullInput
    fullInput = rightOrDie $ parse parseAll "" s

day16b :: String -> String
day16b s = show validNearbyTickets
  where
--    legend = map fromJust $
--      [find (\col -> all (fieldName -> S.member col rulesMap M.!  )
--    rulesMap = rules fullInput
--    cols = [(i, [t!!i | t <- validNearbyTickets]) | i <- [0.. (length (head validNearbyTickets) - 1)]]
    validNearbyTickets = filter (all (`S.member` fullAcceptable)) (nearbyTickets fullInput)
    fullAcceptable = S.unions $ M.elems $ rules fullInput
    fullInput = rightOrDie $ parse parseAll "" s
