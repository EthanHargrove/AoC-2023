import Data.Char
import Data.List
import Data.Maybe
import System.IO

main = do
  result <- getCalValSum "inputs/input1.txt"
  putStrLn ("Solution 1: " ++ show result)
  result2 <- getCalValSum2 "inputs/input1.txt"
  putStrLn ("Solution 2: " ++ show result2)

-- Solution 1: 54644
-- Solution 2: 53348

-- PART 1

getFirstDigit :: [Char] -> Char
getFirstDigit [] = '0'
getFirstDigit (x : xs)
  | isDigit x = x
  | otherwise = getFirstDigit xs

getCalVal :: [Char] -> Int
getCalVal str = 10 * digitToInt (getFirstDigit str) + digitToInt (getFirstDigit (reverse str))

getCalValSum :: FilePath -> IO Int
getCalValSum filePath = do
  contents <- readFile filePath
  let strLines = lines contents
  let results = map getCalVal strLines
  return (sum results)

-- Part 2

numberWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

getFirstDigit2 :: [Char] -> Int
getFirstDigit2 [] = 0
getFirstDigit2 (x1 : x2 : x3 : x4 : x5 : xs)
  | [x1, x2, x3, x4, x5] `elem` numberWords = 1 + fromJust (elemIndex [x1, x2, x3, x4, x5] numberWords)
getFirstDigit2 (x1 : x2 : x3 : x4 : xs)
  | [x1, x2, x3, x4] `elem` numberWords = 1 + fromJust (elemIndex [x1, x2, x3, x4] numberWords)
getFirstDigit2 (x1 : x2 : x3 : xs)
  | [x1, x2, x3] `elem` numberWords = 1 + fromJust (elemIndex [x1, x2, x3] numberWords)
getFirstDigit2 (x : xs)
  | isDigit x = digitToInt x
  | otherwise = getFirstDigit2 xs

getLastDigit2 :: [Char] -> Int
getLastDigit2 [] = 0
getLastDigit2 (x1 : x2 : x3 : x4 : x5 : xs)
  | [x1, x2, x3, x4, x5] `elem` map reverse numberWords = 1 + fromJust (elemIndex (reverse [x1, x2, x3, x4, x5]) numberWords)
getLastDigit2 (x1 : x2 : x3 : x4 : xs)
  | [x1, x2, x3, x4] `elem` map reverse numberWords = 1 + fromJust (elemIndex (reverse [x1, x2, x3, x4]) numberWords)
getLastDigit2 (x1 : x2 : x3 : xs)
  | [x1, x2, x3] `elem` map reverse numberWords = 1 + fromJust (elemIndex (reverse [x1, x2, x3]) numberWords)
getLastDigit2 (x : xs)
  | isDigit x = digitToInt x
  | otherwise = getLastDigit2 xs

getCalVal2 :: [Char] -> Int
getCalVal2 str = 10 * getFirstDigit2 str + getLastDigit2 (reverse str)

getCalValSum2 :: FilePath -> IO Int
getCalValSum2 filePath = do
  contents <- readFile filePath
  let strLines = lines contents
  let results = map getCalVal2 strLines
  return (sum results)