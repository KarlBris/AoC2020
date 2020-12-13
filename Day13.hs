module Day13 where

import Testrunner
import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read

part1 :: String -> Integer
part1 i = toInteger $ b * mins
  where (t, bs) = processInput i
        bs' = catMaybes bs
        m = map (checkSchedule t) bs'
        (b, mins) = minimumBy (\(_,m1) (_,m2) -> if m1 < m2 then LT else GT) m

part2 :: String -> Integer
part2 i = toInteger $ findEarliestSequence 0 inputs
  where (_, bs) = processInput i
        zipped = zip bs [0..]
        inputs = catMaybes $ map (f) zipped


f :: (Maybe Int, Int) -> (Maybe (Int, Int))
f (Nothing, i) = Nothing
f (Just a, i) = Just (a, i)



findEarliestSequence :: Int -> [(Int, Int)] -> Int
findEarliestSequence n ns = if done then n else findEarliestSequence (n+step) ns
  where m = map (checkDeparture n) ns
        step = product (1:(matches m))
        done = sum (map fst m) == 0

checkDeparture :: Int -> (Int, Int) -> (Int, Int)
checkDeparture n (b, phase) = ((n+phase) `mod` b, b)

matches :: [(Int, Int)] -> [Int]
matches [] = []
matches ((m, b):ms) = case m of 0         -> b:(matches ms)
                                otherwise -> matches ms

checkSchedule :: Int -> Int -> (Int, Int)
checkSchedule t b = (b, b - (t `mod` b))

processInput :: String -> (Int, [Maybe Int])
processInput i = (read $ l!!0, map readMaybe $ splitOn "," (l!!1))
  where l = lines i





test1 = compareToExampleSolutions part1 examples13 solutions13_1
test2 = compareToExampleSolutions part2 examples13 solutions13_2
go1   = part1 input13
go2   = part2 input13