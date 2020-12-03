module Lib where

import Days.Day01 (day01a, day01b)
import Days.Day02 (day02a, day02b)
import Days.Day03 (day03a, day03b)

daysMapping :: String -> (String -> String, String -> String)
daysMapping "01" = (day01a, day01b)
daysMapping "02" = (day02a, day02b)
daysMapping "03" = (day03a, day03b)
daysMapping _    = error "not implemented"
