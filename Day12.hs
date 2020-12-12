module Day12 where

import Testrunner

type Pos = (Char, (Int, Int))
type Pos2 = ((Int, Int), (Int, Int))
type Dir = Char

part1 :: String -> Integer
part1 i = toInteger $ abs x + abs y
  where ins = processInput i
        (d,(x,y)) = followInstructions ('E', (0,0)) ins

part2 :: String -> Integer
part2 i = toInteger $ abs x + abs y
  where ins = processInput i
        ((x,y),(wx,wy)) = followWaypoint ((0,0), (10,1)) ins

processInput :: String -> [(Char, Int)]
processInput i = gs
  where gs = map (\l -> (head l, read $ tail l :: Int)) (lines i)

followInstructions :: Pos -> [(Char, Int)] -> Pos
followInstructions p [] = p
followInstructions (d, (x,y)) (('N',a):is) = followInstructions (d, (x,y+a)) is
followInstructions (d, (x,y)) (('S',a):is) = followInstructions (d, (x,y-a)) is
followInstructions (d, (x,y)) (('E',a):is) = followInstructions (d, (x+a,y)) is
followInstructions (d, (x,y)) (('W',a):is) = followInstructions (d, (x-a,y)) is
followInstructions (d, (x,y)) (('F',a):is) = followInstructions (d, (x,y)) ((d,a):is)
followInstructions (d, (x,y)) (('L',a):is) = followInstructions ((dropWhile (/= d) left)!!(turn a), (x,y)) is
followInstructions (d, (x,y)) (('R',a):is) = followInstructions ((dropWhile (/= d) right)!!(turn a), (x,y)) is

followWaypoint :: Pos2 -> [(Char, Int)] -> Pos2
followWaypoint p [] = p
followWaypoint (p, (wx,wy)) (('N',a):is) = followWaypoint (p, (wx,wy+a)) is
followWaypoint (p, (wx,wy)) (('S',a):is) = followWaypoint (p, (wx,wy-a)) is
followWaypoint (p, (wx,wy)) (('E',a):is) = followWaypoint (p, (wx+a,wy)) is
followWaypoint (p, (wx,wy)) (('W',a):is) = followWaypoint (p, (wx-a,wy)) is
followWaypoint ((x,y), (wx,wy)) (('F',a):is) = followWaypoint ((x+(wx*a), (y+(wy*a))), (wx,wy)) is
followWaypoint (p, (wx,wy)) ((d,a):is) = followWaypoint (p, (iterate (rotate d) (wx,wy))!!(turn a)) is

rotate :: Char -> (Int, Int) -> (Int, Int)
rotate 'R' (x,y) = (y,-x)
rotate 'L' (x,y) = (-y,x)

turn :: Int -> Int
turn x = turns
  where turns = ((x `mod` 360) `div` 90)

right = concat $ repeat ['N', 'E', 'S', 'W']
left  = concat $ repeat ['N', 'W', 'S', 'E'] 
