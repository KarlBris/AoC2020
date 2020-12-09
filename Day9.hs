module Day9 where

import Testrunner
import Data.List.Split

part1 :: String -> Integer
part1 i = checkNumbers (take n is) (drop n is)
  where (is, n) = processInput i

checkNumbers :: [Integer] -> [Integer] -> Integer
checkNumbers pre (i:is) = if (length list) == 0 then i else checkNumbers ((tail pre) ++ [i]) is
  where list = [ x + y | x <- pre, y <- pre , x /= y, x + y == i ]

part2 :: String -> Integer
part2 i = (minimum contigList) + (maximum contigList)
  where target = part1 i
        (input, _) = processInput i
        contigList = findContig input target 0 1

findContig :: [Integer] -> Integer -> Int -> Int -> [Integer]
findContig is target from to = if listSum < target then findContig is target from (to+1) else (if listSum > target then findContig is target (from+1) (from+2) else sublist)
  where listSum = sum sublist
        sublist = drop from . take (to+1) $ is

processInput :: String -> ([Integer], Int)
processInput i = (numbers, read count)
  where (count:rest:[]) = splitOn ":" i
        numbers = map read $ lines rest
