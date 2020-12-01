module Day1 where

import Testrunner

part1 :: String -> Integer
part1 i = head productList
  where es = map read $ lines i
        productList = [a * b | a <- es, b <- es, a + b == 2020]

part2 :: String -> Integer
part2 i = head productList
  where es = map read $ lines i
        productList = [a * b * c | a <- es, b <- es, c <- es, a + b + c == 2020]
