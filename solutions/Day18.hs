

main :: IO ()
main = do
    result1 <- getResult1 "inputs/input1811.txt"
    print result1
    -- result2 <- getResult2 "inputs/input18.txt"
    -- print result2
    -- Solution 1: 1581679977
    -- SOlution 2: 889

-- Part 1
data Point = Point {xCoord :: Int, yCoord :: Int} deriving Show

getVertices :: [[String]] -> [Point]
getVertices [] = []
getVertices commands = getVerticesAux [Point {xCoord=0, yCoord=0}] (Point {xCoord=0, yCoord=0}) commands
    where
        getVerticesAux :: [Point] -> Point -> [[String]] -> [Point]
        getVerticesAux points _ [] = points
        getVerticesAux points lastPoint (x:xs)
            | head x == "L" = getVerticesAux (points ++ [Point {xCoord=xCoord lastPoint - (read (x !! 1) :: Int), yCoord=yCoord lastPoint}]) (Point {xCoord=xCoord lastPoint - (read (x !! 1) :: Int), yCoord=yCoord lastPoint}) xs
            | head x == "R" = getVerticesAux (points ++ [Point {xCoord=xCoord lastPoint + (read (x !! 1) :: Int), yCoord=yCoord lastPoint}]) (Point {xCoord=xCoord lastPoint + (read (x !! 1) :: Int), yCoord=yCoord lastPoint}) xs
            | head x == "U" = getVerticesAux (points ++ [Point {xCoord=xCoord lastPoint, yCoord=yCoord lastPoint + (read (x !! 1) :: Int)}]) (Point {xCoord=xCoord lastPoint, yCoord=yCoord lastPoint + (read (x !! 1) :: Int)}) xs
            | head x == "D" = getVerticesAux (points ++ [Point {xCoord=xCoord lastPoint, yCoord=yCoord lastPoint - (read (x !! 1) :: Int)}]) (Point {xCoord=xCoord lastPoint, yCoord=yCoord lastPoint - (read (x !! 1) :: Int)}) xs

shoelace :: [Point] -> Int
shoelace vertices = abs (sum terms) `div` 2
    where
    n = length vertices
    terms = zipWith (\(Point x1 y1) (Point x2 y2) -> x1 * y2 - x2 * y1) vertices (rotateList vertices)

rotateList :: [Point] -> [Point]
rotateList [] = []
rotateList (x:xs) = xs ++ [x]

getResult1 :: FilePath -> IO Int
getResult1 filePath = do
    contents <- readFile filePath
    let strLines = lines contents
    let commands = map words strLines
    let vertices = getVertices commands
    let area = shoelace vertices
    print area
    return 9

-- Part 2