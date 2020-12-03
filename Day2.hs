module Day2 where

import Testrunner

part1 :: String -> Integer
part1 i = toInteger $ length $ filter (==True) $ map isPasswordValid (splitStuff i) 

splitStuff :: String -> [(((String, String),Char),String)]
splitStuff i = split''
  where passLines = lines i
        split = map (splitString ':' []) passLines
        split' = map (\(l, r) -> (splitString ' ' [] l, tail r)) split
        split'' = map (\((ll, lr),r) -> (((splitString '-' [] ll),head lr), r)) split'

isPasswordValid :: (((String, String),Char),String) -> Bool
isPasswordValid (((min, max), c), pass) = occurrences >= (read min) && occurrences <= (read max)
  where occurrences = length $ filter (==c) pass

part2 :: String -> Integer
part2 i = toInteger $ length $ filter (==True) $ map isPasswordValid2 (splitStuff i) 

isPasswordValid2 :: (((String, String),Char),String) -> Bool
isPasswordValid2 (((a, b), c), pass) = (pass!!((read a)-1) == c) `xor` (pass!!((read b)-1) == c)

xor True False = True
xor False True = True
xor _ _        = False

splitString c ls (r:rs)
  | r == c = (reverse ls, rs)
  | otherwise = splitString c (r:ls) rs 
splitString c ls []       = (ls, [])
