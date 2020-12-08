module Day7 where

import Testrunner
import Data.List.Split
import Data.List
import Data.Char


type BagRule = (String, [(Integer, String)])

part1 :: String -> Integer
part1 i = toInteger $ length $ filter (==True) cts
  where brs = processInput i
        cts = map (canContain brs "shiny gold") brs

part2 :: String -> Integer
part2 i = hmc
  where brs = processInput i
        hmc = howManyContains brs "shiny gold"

howManyContains abrs s = if rs == [] then 0 else sum $ map (sumRule abrs) rs
  where (n, rs) = head $ filter (\(br,_) -> s==br) abrs

sumRule :: [BagRule] -> (Integer, String) -> Integer
sumRule abrs (i,s) = i + (i * howManyContains abrs s)

canContain :: [BagRule] -> String -> BagRule -> Bool
canContain abrs s (_, [])    = False
canContain abrs s (i,((i2, br):brs)) = if br == s then True else (or $ map (canContain abrs s) matchingAbrs ) || canContain abrs s (i,brs)
  where matchingAbrs = filter (\(s,_) -> s==br) abrs


processInput :: String -> [BagRule]
processInput i = map makeBagRule i3
  where i2 = map (splitOn " bags contain ") (lines i)
        i3 = map (concat . map (splitOn ", ")) i2

makeBagRule :: [String] -> BagRule
makeBagRule s = (s!!0, makeRules (tail s))

makeRules :: [String] -> [(Integer, String)]
makeRules ("no other bags.":[]) = []
makeRules rs = map (\r -> (toInteger $ digitToInt $ r!!0, concat $ intersperse " " $ take 2 $ tail $ splitOn " " r)) rs
