module Day10 where

import Testrunner
import Data.List
import Data.List.Split

part1 :: String -> Integer
part1 i = toInteger $ (length $ filter (==1) joltageJumps) * (length $ filter (==3) joltageJumps)
  where joltageJumps = countJoltages [] (processInput i)

processInput :: String -> [Integer]
processInput s = 0:joltages
  where joltages  = sort $ map (read) $ lines s

countJoltages :: [Integer] -> [Integer] -> [Integer]
countJoltages l (j1:j2:js) = if j2 - j1 == 1 then countJoltages (1:l) (j2:js) else countJoltages (3:l) (j2:js)
countJoltages l (_:[]) = reverse (3:l)

part2 :: String -> Integer
part2 i = product $ map f lengths
  where jolts2 = countJoltages [] (processInput i)
        joltsSplit = splitOn [3] jolts2
        lengths = map length joltsSplit

f 0 = 1
f 1 = 1
f 2 = 2
f 3 = 4
f 4 = 7
f 5 = 13

test1 = compareToExampleSolutions part1 examples10 solutions10_1
test2 = compareToExampleSolutions part2 examples10 solutions10_2
go1   = part1 input10
go2   = part2 input10