main :: IO ()
main = do
    result1 <- getResult1 "inputs/input9.txt"
    print result1
    result2 <- getResult2 "inputs/input9.txt"
    print result2
    -- Solution 1: 1581679977
    -- SOlution 2: 889

-- Part 1
getDifferences :: [Int] -> [Int]
getDifferences [] = []
getDifferences (x1:x2:xs) = (x2 - x1) : getDifferences (x2:xs)
getDifferences _ = []

nextNum :: [Int] -> Int
nextNum listNums
    | all (==0) listNums = 0
    | otherwise = last listNums + nextNum (getDifferences listNums)

getResult1 :: FilePath -> IO Int
getResult1 filePath = do
    contents <- readFile filePath
    let strNums = map words (lines contents)
    let listNums = map (map read) strNums
    let nextNums = map nextNum listNums
    return (sum nextNums)

-- Part 2
getResult2 :: FilePath -> IO Int
getResult2 filePath = do
    contents <- readFile filePath
    let strNums = map words (lines contents)
    let listNums = map (reverse.map read) strNums
    let nextNums = map nextNum listNums
    return (sum nextNums)