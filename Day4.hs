module Day4 where

import Testrunner
import Text.Regex.TDFA
import Data.List.Split

part1 :: String -> Integer
part1 i = toInteger $ length $ filter (==True) $ map hasMandatoryFields (processInput i)

part2 :: String -> Integer
part2 i = toInteger $ length $ filter (==True) $ map isPassportValid (processInput i)

processInput :: String -> [[String]]
processInput i = i2
  where i1 = splitOn "\n\n" i
        i2 = map (splitOneOf" \n") i1

hasMandatoryFields :: [String] -> Bool
hasMandatoryFields pp = and $ map (\f -> f `elem` a) mandatoryFields
  where a = map field pp

field :: String -> String
field s = head $ splitOn ":" s

isPassportValid :: [String] -> Bool
isPassportValid pp = (hasMandatoryFields pp) && (and $ map (\f -> validateField $ splitField f) pp)

splitField :: String -> (String, String)
splitField s = (split!!0, split!!1)
  where split = splitOn ":" s

validateField :: (String, String) -> Bool
validateField ("byr", val) = (length val == 4) && (read val >= 1920) && (read val <= 2002) 
validateField ("iyr", val) = (length val == 4) && (read val >= 2010) && (read val <= 2020)
validateField ("eyr", val) = (length val == 4) && (read val >= 2020) && (read val <= 2030)
validateField ("hgt", val) = case unit of "cm" -> (value >= 150) && (value <= 193)
                                          "in" -> (value >= 59) && (value <= 76)
                                          _    -> False 
  where unit = drop (length val - 2) val
        value = read $ take (length val - 2) val
validateField ("hcl", val) = val =~ "#[abcdef0123456789]{6}" && length val == 7 
validateField ("ecl", val) = val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] 
validateField ("pid", val) = val =~ "[0-9]{9}" && length val == 9
validateField ("cid", val) = True

mandatoryFields = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
