module Day8 where

import Testrunner

data Instruction = Nop Integer | Jmp Integer | Acc Integer
instance Show Instruction 
  where show (Nop i) = "nop " ++ (show i)
        show (Jmp i) = "jmp " ++ (show i)
        show (Acc i) = "acc " ++ (show i)

part1 :: String -> Integer
part1 i = stepProgram (processInput i) [] 0 0

part2 :: String -> Integer
part2 i = head $ filter (/= (-1)) $ map (\p -> stepProgram2 p [] 0 0) programs
  where input = processInput i
        programs = foo [] input [input]

foo :: [Instruction] -> [Instruction] -> [[Instruction]] -> [[Instruction]]
foo pis (i:is) acc = case i of Jmp a -> foo (i:pis) is (((reverse pis) ++ (Nop a):is):acc)
                               Nop a -> foo (i:pis) is (((reverse pis) ++ (Jmp a):is):acc)
                               Acc a -> foo (i:pis) is acc
foo pis [] acc = acc

--               program         visited     acc         pc      return acc
stepProgram :: [Instruction] -> [Integer] -> Integer -> Integer -> Integer
stepProgram is vs acc pc = if pc `elem` vs then acc else case is!!(fromInteger pc) of (Nop i) -> stepProgram is (pc:vs) acc (pc+1)
                                                                                      (Jmp i) -> stepProgram is (pc:vs) acc (pc+i)
                                                                                      (Acc i) -> stepProgram is (pc:vs) (acc + i) (pc+1)

--               program         visited     acc         pc      return acc
stepProgram2 :: [Instruction] -> [Integer] -> Integer -> Integer -> Integer
stepProgram2 is vs acc pc = if pc `elem` vs 
  then (-1)
  else if fromInteger pc >= length is then acc else case is!!(fromInteger pc) of (Nop i) -> stepProgram2 is (pc:vs) acc (pc+1)
                                                                                 (Jmp i) -> stepProgram2 is (pc:vs) acc (pc+i) 
                                                                                 (Acc i) -> stepProgram2 is (pc:vs) (acc + i) (pc+1) 

processInput :: String -> [Instruction]
processInput i = map makeInstruction i2
  where i2 = map words $ lines i

makeInstruction :: [String] -> Instruction
makeInstruction ("nop":i:[]) = Nop (read i)
makeInstruction ("jmp":i:[]) = Jmp (read i)
makeInstruction ("acc":i:[]) = Acc (read i)
