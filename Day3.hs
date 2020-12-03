module Day3 where

import Testrunner

part1 :: String -> Integer
part1 i = toInteger trees
  where input = processInput i
        zipped = zip input [0,3..]
        trees = length $ filter (==True) $ map isATree zipped

part2 :: String -> Integer
part2 i = toInteger $ product trees
  where input = processInput i
        trees = map (\(right, down) -> length $ filter (==True) $ map isATree (zip (every down input) [0,right..])) slopes

isATree :: (String, Int) -> Bool
isATree (trees, coord) = trees!!coord == '#'

processInput :: String -> [String]
processInput i = map (\a -> concat (repeat a)) treeLines
  where treeLines = lines i

slopes = [(1,1),(3,1),(5,1),(7,1),(1,2)]

every n l = fst $ unzip filtered
  where zipped = zip l [0..]
        filtered = filter (\(l, p) -> p `mod` n == 0) zipped
