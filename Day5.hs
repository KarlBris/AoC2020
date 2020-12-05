module Day5 where

import Testrunner
import Data.List

part1 :: String -> Integer
part1 i = maximum nums
  where passes = processInput i
        nums = map (\(rowCode, colCode) -> ((makeRow rowCode [0..127]) * 8) + (makeColumn colCode [0..7])) passes

part2 :: String -> Integer
part2 i = (fst $ head $ filter (\(_,b) -> b) foo) + 1
  where passes = processInput i
        sortedNums = sort $ map (\(rowCode, colCode) -> ((makeRow rowCode [0..127]) * 8) + (makeColumn colCode [0..7])) passes
        numComp = zip (0:sortedNums) sortedNums
        foo = map (\(a,b) -> (a, b-a > 1)) (tail numComp)

processInput :: String -> [(String, String)]
processInput i = map (\s -> (take 7 s, drop 7 s)) a
  where a = lines i

makeRow :: [Char] -> [Integer] -> Integer
makeRow ('F':cs) l = makeRow cs (take ((length l) `div` 2) l)
makeRow ('B':cs) l = makeRow cs (drop ((length l) `div` 2) l)
makeRow [] l       = head l

makeColumn :: [Char] -> [Integer] -> Integer
makeColumn ('L':cs) l = makeColumn cs (take ((length l) `div` 2) l)
makeColumn ('R':cs) l = makeColumn cs (drop ((length l) `div` 2) l)
makeColumn [] l       = head l
