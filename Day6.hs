module Day6 where

import Testrunner
import Data.List
import Data.List.Split

part1 :: String -> Integer
part1 i = toInteger $ sum $ map (length . nub . concat) (processInput i)

part2 :: String -> Integer
part2 i =  toInteger $ sum $ map (length . (foldl1 intersect)) (processInput i)

processInput :: String -> [[String]]
processInput i = i2
  where i1 = splitOn "\n\n" i
        i2 = map (splitOn "\n") i1
