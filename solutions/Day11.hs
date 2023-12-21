import Data.List

main :: IO ()
main = do
    result1 <- getResult1 "inputs/input11.txt"
    print result1
    result2 <- getResult2 "inputs/input11.txt"
    print result2
    -- Solution 1: 9627977
    -- SOlution 2: 644248339497

-- Part 1
rowExpansion :: [String] -> [String]
rowExpansion [] = []
rowExpansion (x:xs) 
    | all (== '.') x = replicate 2 x ++ rowExpansion xs
    | otherwise = x : rowExpansion xs

colExpansion :: [[Char]] -> [[Char]]
colExpansion list = transpose (rowExpansion (transpose list))

data Coord = Coord {row :: Int, col :: Int}
    deriving (Show, Eq)

getGalaxyCoords :: [String] -> [Coord]
getGalaxyCoords = getRowCoords 0
    where
        getRowCoords :: Int -> [String] -> [Coord]
        getRowCoords _ [] = []
        getRowCoords rowInd (x:xs) = getColCoords rowInd 0 x ++ getRowCoords (rowInd + 1) xs
            where
                getColCoords :: Int -> Int -> [Char] -> [Coord]
                getColCoords _ _ [] = []
                getColCoords rowInd colInd (x:xs)
                    | x == '#' = Coord rowInd colInd : getColCoords rowInd (colInd + 1) xs
                    | otherwise = getColCoords rowInd (colInd + 1) xs

getDistance :: Coord -> Coord -> Int
getDistance coord1 coord2 = abs (row coord1 - row coord2) + abs (col coord1 - col coord2)

getDistances :: [Coord] -> [Int]
getDistances [] = []
getDistances (x:xs) = map (getDistance x) xs ++ getDistances xs

getResult1 :: FilePath -> IO Int
getResult1 filePath = do
    contents <- readFile filePath
    let strLines = lines contents
    let expandedLines = rowExpansion (colExpansion strLines)
    let coords = getGalaxyCoords expandedLines
    let distances = getDistances coords
    return $ sum distances

-- Part 2
rowExpansion2 :: Int -> [String] -> [String]
rowExpansion2 _ [] = []
rowExpansion2 numRep (x:xs) 
    | all (== '.') x = replicate numRep x ++ rowExpansion2 numRep xs
    | otherwise = x : rowExpansion2 numRep xs

colExpansion2 :: Int -> [[Char]] -> [[Char]]
colExpansion2 numRep list = transpose (rowExpansion2 numRep (transpose list))

data Point = Point { x :: Int, y :: Int }

getSlope :: Point -> Point -> Int
getSlope p1 p2 = (y p2 - y p1) `div` (x p2 - x p1)

getIntercept :: Point -> Int -> Int
getIntercept p1 m = y p1 - m * x p1

getResult2 :: FilePath -> IO Int
getResult2 filePath = do
    contents <- readFile filePath
    let strLines = lines contents
    let expanded2 = rowExpansion2 2 (colExpansion2 2 strLines)
    let coords2 = getGalaxyCoords expanded2
    let distance2 = sum (getDistances coords2)
    let expanded3 = rowExpansion2 3 (colExpansion2 3 strLines)
    let coords3 = getGalaxyCoords expanded3
    let distance3 = sum (getDistances coords3)
    let slope = getSlope (Point 2 distance2) (Point 3 distance3)
    let intercept = getIntercept (Point 2 distance2) slope
    return (slope * 1000000 + intercept)