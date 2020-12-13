module Days.Day04 where

import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S

-- TODO: this could use a real parser combinator

--data MeasurementUnit = INCH | CENTIMETER deriving (Show)
--
--data Passport = Passport
--  { birthYear :: Maybe [Int],
--    issueYear :: Maybe [Int],
--    expYear   :: Maybe [Int],
--    height    :: Maybe [(Int, MeasurementUnit)],
--    hairColor :: Maybe [String],
--    pid       :: Maybe [Int],
--    cid       :: Maybe [String]
--  }
--  deriving (Show)
--constructPassport :: M.Map String String -> Passport
--constructPassport m = Passport
--  { birthYear=read <$> M.lookup "byr" m
--  , issueYear=read <$> M.lookup "iyr" m
--  , expYear=read <$> M.lookup "eyr" m
--  , height=
--  }

listWithSeparatedElementsToMap :: String -> [String] -> M.Map String String
listWithSeparatedElementsToMap sep l = M.fromList $ map asTuple l
  where
    asTuple s = ((splitOn sep s) !! 0, (splitOn sep s) !! 1)

passportsRaw :: String -> [[String]]
passportsRaw = map (words . unwords) . splitOn [""] . lines

passportsAsMaps :: String -> [M.Map String String]
passportsAsMaps = map (listWithSeparatedElementsToMap ":") . passportsRaw

isPassportValid :: M.Map String String -> Bool
isPassportValid m = and [birthYearValid, issueYearValid, expirationYearValid, heightValid, hairColorValid, eyeColorValid, pidValid]
  where
    birthYearValid = getInt "byr" >= 1920 && getInt "byr" <= 2002
    issueYearValid = getInt "iyr" >= 2010 && getInt "iyr" <= 2020
    expirationYearValid = getInt "eyr" >= 2020 && getInt "eyr" <= 2030
    heightValid = case M.lookup "hgt" m of
      Nothing -> False
      Just s -> (unit == "cm" && measure >= 150 && measure <= 193) || (unit == "in" && measure >= 59 && measure <= 76)
        where
          measure = read measureRaw :: Int
          (measureRaw, unit) = splitAt (length s - 2) s
    hairColorValid = case M.lookup "hcl" m of
      Nothing -> False
      Just s -> length s == 7 && head s == '#' && S.isSubsetOf (S.fromList s) (S.fromList "#abcdef0123456789")
    eyeColorValid = case M.lookup "ecl" m of
      Nothing -> False
      Just s -> S.member s $ S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    pidValid = case M.lookup "pid" m of
      Nothing -> False
      Just s -> length s == 9 && S.isSubsetOf (S.fromList s) (S.fromList "0123456789")
    getInt k = read (M.findWithDefault "-1" k m) :: Int

day04a :: String -> String
day04a = show . length . filter (S.isSubsetOf required . M.keysSet) . passportsAsMaps
  where
    required = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

day04b :: String -> String
day04b = show . length . filter isPassportValid . passportsAsMaps
