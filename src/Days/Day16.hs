module Days.Day16 where

import Days.Common (rightOrDie)
import Data.List (isPrefixOf, transpose)
import Text.Parsec
import qualified Data.Map as M
import qualified Data.Set as S

data FullInput =
  FullInput
  { rules :: M.Map String (S.Set Int)
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
  return FullInput
         { rules = M.fromList rulesList
         , myTicket=myTicketIn
         , nearbyTickets=nearbyTicketsIn 
         }
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

prune :: (Eq a, Ord a) => [S.Set a] -> [S.Set a]
prune l = map removeElements l
  where
    removeElements s = if S.size s > 1 then S.difference s singletons else s
    singletons = S.unions $ filter (\s -> S.size s == 1) l

day16b :: String -> String
day16b s = show $ product $ map (\k -> myTicket fullInput !! k) eligible
  where
    eligible = map fst $ filter (\(_,name) -> "depart" `isPrefixOf` name) (zip [0 .. ] legend)
    legend = map (S.elemAt 0) $ until (all (\ s' -> S.size s' == 1)) prune candidates
    candidates = map
                   (S.fromList . map snd . filter (\ (c, k) -> allFulfill k c))
                   ([[(col, key) | key <- M.keys rulesMap] | col <- cols])
    allFulfill key col = all (\n -> S.member n (rulesMap M.! key)) col
    rulesMap = rules fullInput
    cols = transpose validNearbyTickets
    validNearbyTickets = filter (all (`S.member` fullAcceptable)) (nearbyTickets fullInput)
    fullAcceptable = S.unions $ M.elems $ rules fullInput
    fullInput = rightOrDie $ parse parseAll "" s
