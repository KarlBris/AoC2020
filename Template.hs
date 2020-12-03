module Day? where

import Testrunner
import Text.Regex.TDFA

part1 :: String -> Integer
part1 i = read i

part2 :: String -> Integer
part2 i = undefined






test1 = compareToExampleSolutions part1 examples? solutions?_1
test2 = compareToExampleSolutions part2 examples? solutions?_2
go1   = part1 input?
go2   = part2 input?
-- tail $ getAllTextSubmatches ((examples2!!0) =~ "(.*)-(.*) (.{1}): (.*)") :: [String]