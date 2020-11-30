module Testrunner where

runExamples :: (String -> Integer) -> [String] -> [Integer]
runExamples f examples = map f examples

compareToExampleSolutions :: (String -> Integer) -> [String] -> [Integer] -> IO()
compareToExampleSolutions f examples exampleSolutions = do
  let comparisonStrings = map makeComparisonString (makeComparison (runExamples f examples) exampleSolutions)
  putStrLn (concat comparisonStrings)

makeComparison :: [Integer] -> [Integer] -> [(Integer, Integer, Bool)]
makeComparison mySolutions exampleSolutions = map (\(mySol, exSol) -> (mySol, exSol, mySol == exSol)) a
  where a = zip mySolutions exampleSolutions

makeComparisonString :: (Integer, Integer, Bool) -> String
makeComparisonString (mySol, exSol, b)  = "Expected " ++ (show exSol) ++ ", received " ++ (show mySol) ++ s
  where s = if b then ". Correct!\n" else ". INCORRECT!\n"

--Day1
input1 = undefined
examples1 = [undefined]
solutions1 = [undefined]

--Day2
input2 = undefined
examples2 = [undefined]
solutions2 = [undefined]

--Day3
input3 = undefined
examples3 = [undefined]
solutions3 = [undefined]

--Day4
input4 = undefined
examples4 = [undefined]
solutions4 = [undefined]

--Day5
input5 = undefined
examples5 = [undefined]
solutions5 = [undefined]