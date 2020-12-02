module Lib where

import Days.Day01 (day01a, day01b)


daysMapping :: String -> (String -> String, String -> String)
daysMapping "01" = (day01a, day01b)
daysMapping _    = error "not implemented"
