import Data.Char
import Text.Read

main = do
  result <- getPossGameNums "inputs/input2.txt"
  putStrLn ("Solution 1: " ++ show result)
  result2 <- getSumGamePowers "inputs/input2.txt"
  putStrLn ("Solution 2: " ++ show result2)

-- Solution 1: 2278
-- Solution 2: 67953

-- Part 1

removePunctuation :: String -> String
removePunctuation = filter (\c -> isAlphaNum c || c == ' ')

isPossible :: [String] -> Bool
isPossible [] = True
isPossible (x1 : x2 : xs)
  | all isDigit x1 && read x1 > 14 && x2 == "blue" = False
  | all isDigit x1 && read x1 > 13 && x2 == "green" = False
  | all isDigit x1 && read x1 > 12 && x2 == "red" = False
  | otherwise = isPossible (x2 : xs)
isPossible _ = True

getGameNumber :: [String] -> Int
getGameNumber strList = gameNumberAux strList (isPossible strList)
  where
    gameNumberAux :: [String] -> Bool -> Int
    gameNumberAux (x1 : x2 : xs) True
      | x1 == "Game" = read x2
    gameNumberAux _ _ = 0

getPossGameNums :: FilePath -> IO Int
getPossGameNums filePath = do
  contents <- readFile filePath
  let strLines = map (words . removePunctuation) (lines contents)
  let results = map getGameNumber strLines
  return (sum results)

-- Part 2

getGamePower :: [String] -> Int
getGamePower strList = gamePowerAux strList 0 0 0
  where
    gamePowerAux :: [String] -> Int -> Int -> Int -> Int
    gamePowerAux [] blue green red = blue * green * red
    gamePowerAux (x1 : x2 : xs) blue green red
      | all isDigit x1 && read x1 > blue && x2 == "blue" = gamePowerAux xs (read x1) green red
      | all isDigit x1 && read x1 > green && x2 == "green" = gamePowerAux xs blue (read x1) red
      | all isDigit x1 && read x1 > red && x2 == "red" = gamePowerAux xs blue green (read x1)
      | otherwise = gamePowerAux (x2 : xs) blue green red
    gamePowerAux _ blue green red = blue * green * red

getSumGamePowers :: FilePath -> IO Int
getSumGamePowers filePath = do
  contents <- readFile filePath
  let strLines = map (words . removePunctuation) (lines contents)
  let results = map getGamePower strLines
  return (sum results)