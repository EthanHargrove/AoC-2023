import Data.List
import Data.Maybe

main = do
  result <- getResult1 "inputs/input4.txt"
  putStrLn ("Solution 1: " ++ show result)

-- Solution 1: 21558

getNumMatches :: [Char] -> Int
getNumMatches str = getMatchesAux str (fromJust (elemIndex ':' str)) (fromJust (elemIndex '|' str))
  where
    getMatchesAux :: [Char] -> Int -> Int -> Int
    getMatchesAux str start split = length (words (take (split - start) (drop start str)) `intersect` words (drop split str))

getPoints :: [Char] -> Int
getPoints str = getPointsAux (getNumMatches str)
  where
    getPointsAux :: Int -> Int
    getPointsAux numMatches
      | numMatches == 0 = 0
      | otherwise = 2 ^ (numMatches - 1)

getResult1 :: FilePath -> IO Int
getResult1 filePath = do
  contents <- readFile filePath
  let strLines = lines contents
  let result = map getPoints strLines
  return (sum result)