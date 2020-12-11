module Day11 where

import Testrunner
import Data.Matrix
import Data.Maybe

part1 :: String -> Integer
part1 i = toInteger $ length $ filter (=='#') $ toList $ converge step $ processInput i

part2 :: String -> Integer
part2 i = toInteger $ length $ filter (=='#') $ toList $ converge step2 $ processInput i

processInput :: String -> Matrix Char
processInput i = fromLists $ lines i

step :: Matrix Char -> Matrix Char
step seats = mapPos (bar sumAdjacentOccupied seats 4) seats

step2 :: Matrix Char -> Matrix Char
step2 seats = mapPos (bar sumAdjacentOccupiedLong seats 5) seats

bar :: ((Int, Int) -> Matrix Char -> Int) -> Matrix Char -> Int -> (Int, Int) -> Char -> Char
bar f m n pos seat = case seat of 'L'       -> if adjacentOccupied == 0 then '#' else 'L'
                                  '#'       -> if adjacentOccupied >= n then 'L' else '#'
                                  otherwise -> seat
  where adjacentOccupied = f pos m

converge :: (Matrix Char -> Matrix Char) -> Matrix Char -> Matrix Char
converge f m = if m == m' then m else converge f m'
  where m' = f m

sumAdjacentOccupied :: (Int, Int) -> Matrix Char -> Int
sumAdjacentOccupied (a,b) m = length $ filter (=='#') $ catMaybes [safeGet a' b' m | a' <- [a-1, a, a+1], b' <- [b-1, b, b+1], not ((a' == a) && (b' == b))]

sumAdjacentOccupiedLong :: (Int, Int) -> Matrix Char -> Int
sumAdjacentOccupiedLong (a,b) m = length $ filter (=='#') $ seenSeats
  where u = catMaybes $ takeWhile (isJust) [safeGet a' b m | a' <- [(a-1),(a-2)..]]
        d = catMaybes $ takeWhile (isJust) [safeGet a' b m | a' <- [(a+1)..]]
        l = catMaybes $ takeWhile (isJust) [safeGet a b' m | b' <- [b-1,b-2..]]
        r = catMaybes $ takeWhile (isJust) [safeGet a b' m | b' <- [b+1..]]
        ul = catMaybes $ takeWhile (isJust) [safeGet a' b' m | (a',b') <- zip [(a-1),(a-2)..] [b-1,b-2..]]
        ur = catMaybes $ takeWhile (isJust) [safeGet a' b' m | (a',b') <- zip [(a-1),(a-2)..] [b+1..]]
        dl = catMaybes $ takeWhile (isJust) [safeGet a' b' m | (a',b') <- zip [(a+1)..] [b-1,b-2..]]
        dr = catMaybes $ takeWhile (isJust) [safeGet a' b' m | (a',b') <- zip [(a+1)..] [b+1..]]
        first l = if filter ( /= '.') l == [] then '.' else head $ filter ( /= '.') l
        seenSeats = [first u, first d, first l, first r, first ul, first ur, first dl, first dr]