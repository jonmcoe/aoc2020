module Lib where

import Days.Day01 (day01a, day01b)
import Days.Day02 (day02a, day02b)


daysMapping :: String -> (String -> String, String -> String)
daysMapping "01" = (day01a, day01b)
daysMapping "02" = (day02a, day02b)
daysMapping _    = error "not implemented"
