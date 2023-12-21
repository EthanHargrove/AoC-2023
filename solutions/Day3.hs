import Data.Char
import Data.List

main = do
  result <- getResult1 "inputs/input3.txt"
  putStrLn ("Solution 1: " ++ show result)
  result2 <- getResult2 "inputs/input3.txt"
  putStrLn ("Solution 2: " ++ show result2)

-- Solution 1: 521515
-- Solution 2: 69527306

-- Part 2

data Coord = Coord {row :: Int, col :: Int}
  deriving (Show, Eq)

getSymbolCoords :: [String] -> [Coord]
getSymbolCoords = getRowSymbols 0
  where
    getRowSymbols :: Int -> [String] -> [Coord]
    getRowSymbols _ [] = []
    getRowSymbols rowInd (x : xs) = getColSymbols rowInd 0 x ++ getRowSymbols (rowInd + 1) xs
      where
        getColSymbols :: Int -> Int -> [Char] -> [Coord]
        getColSymbols _ _ [] = []
        getColSymbols rowInd colInd (x : xs)
          | x /= '.' && not (isDigit x) = Coord rowInd colInd : getColSymbols rowInd (colInd + 1) xs
          | otherwise = getColSymbols rowInd (colInd + 1) xs

getAdjacentNums :: [String] -> Coord -> [Int]
getAdjacentNums rows symbolCoord = filter (/= 0) (map (\s -> if null s then 0 else read s) (getLeftNum rows symbolCoord ++ getRightNum rows symbolCoord ++ getTopNums rows symbolCoord ++ getBottomNums rows symbolCoord))

getLeftNum :: [String] -> Coord -> [[Char]]
getLeftNum rows symbolCoord = [reverse (getFirstNumStr (reverse (take (col symbolCoord) (rows !! row symbolCoord))))]

getRightNum :: [String] -> Coord -> [[Char]]
getRightNum rows symbolCoord = [getFirstNumStr (drop (col symbolCoord + 1) (rows !! row symbolCoord))]

getTopLeftNum :: [String] -> Coord -> [Char]
getTopLeftNum rows symbolCoord
  | row symbolCoord == 0 = []
  | otherwise = reverse (getFirstNumStr (reverse (take (col symbolCoord) (rows !! (row symbolCoord - 1)))))

getTopRightNum :: [String] -> Coord -> [Char]
getTopRightNum rows symbolCoord
  | row symbolCoord == 0 = []
  | otherwise = getFirstNumStr (drop (col symbolCoord + 1) (rows !! (row symbolCoord - 1)))

getTopNums :: [String] -> Coord -> [[Char]]
getTopNums rows symbolCoord
  | row symbolCoord == 0 = []
  | isDigit (rows !! (row symbolCoord - 1) !! col symbolCoord) = [getTopLeftNum rows symbolCoord ++ [rows !! (row symbolCoord - 1) !! col symbolCoord] ++ getTopRightNum rows symbolCoord]
  | otherwise = getTopLeftNum rows symbolCoord : [getTopRightNum rows symbolCoord]

getBottomLeftNum :: [String] -> Coord -> [Char]
getBottomLeftNum rows symbolCoord
  | row symbolCoord > (length rows + 1) = []
  | otherwise = reverse (getFirstNumStr (reverse (take (col symbolCoord) (rows !! (row symbolCoord + 1)))))

getBottomRightNum :: [String] -> Coord -> [Char]
getBottomRightNum rows symbolCoord
  | row symbolCoord > (length rows + 1) = []
  | otherwise = getFirstNumStr (drop (col symbolCoord + 1) (rows !! (row symbolCoord + 1)))

getBottomNums :: [String] -> Coord -> [[Char]]
getBottomNums rows symbolCoord
  | row symbolCoord == 0 = []
  | isDigit (rows !! (row symbolCoord + 1) !! col symbolCoord) = [getBottomLeftNum rows symbolCoord ++ [rows !! (row symbolCoord + 1) !! col symbolCoord] ++ getBottomRightNum rows symbolCoord]
  | otherwise = getBottomLeftNum rows symbolCoord : [getBottomRightNum rows symbolCoord]

getFirstNumStr :: [Char] -> [Char]
getFirstNumStr = firstNumStrAux []
  where
    firstNumStrAux :: [Char] -> [Char] -> [Char]
    firstNumStrAux numStr (x : xs)
      | isDigit x = firstNumStrAux (numStr ++ [x]) xs
      | otherwise = numStr
    firstNumStrAux numStr [] = numStr

getResult1 :: FilePath -> IO Int
getResult1 filePath = do
  contents <- readFile filePath
  let strLines = lines contents
  let coords = getSymbolCoords strLines
  let result = concatMap (getAdjacentNums strLines) coords
  return (sum result)

-- Part 2

getGearCoords :: [String] -> [Coord]
getGearCoords = getRowSymbols 0
  where
    getRowSymbols :: Int -> [String] -> [Coord]
    getRowSymbols _ [] = []
    getRowSymbols rowInd (x : xs) = getColSymbols rowInd 0 x ++ getRowSymbols (rowInd + 1) xs
      where
        getColSymbols :: Int -> Int -> [Char] -> [Coord]
        getColSymbols _ _ [] = []
        getColSymbols rowInd colInd (x : xs)
          | x == '*' = Coord rowInd colInd : getColSymbols rowInd (colInd + 1) xs
          | otherwise = getColSymbols rowInd (colInd + 1) xs

getResult2 :: FilePath -> IO Int
getResult2 filePath = do
  contents <- readFile filePath
  let strLines = lines contents
  let coords = getGearCoords strLines
  let result = map product (filter (\s -> length s == 2) (map (getAdjacentNums strLines) coords))
  return (sum result)