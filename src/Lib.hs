module Lib where

import Days.Day01 (day01a, day01b)
import Days.Day02 (day02a, day02b)
import Days.Day03 (day03a, day03b)
import Days.Day04 (day04a, day04b)
import Days.Day05 (day05a, day05b)
import Days.Day06 (day06a, day06b)
import Days.Day07 (day07a, day07b)
import Days.Day08 (day08a, day08b)
import Days.Day09 (day09a, day09b)

daysMapping :: String -> (String -> String, String -> String)
daysMapping "01" = (day01a, day01b)
daysMapping "02" = (day02a, day02b)
daysMapping "03" = (day03a, day03b)
daysMapping "04" = (day04a, day04b)
daysMapping "05" = (day05a, day05b)
daysMapping "06" = (day06a, day06b)
daysMapping "07" = (day07a, day07b)
daysMapping "08" = (day08a, day08b)
daysMapping "09" = (day09a, day09b)
daysMapping _    = error "not implemented"
