import Data.List

main :: IO ()
main = do
    result1 <- getResult1 "inputs/input13.txt"
    print result1
    result2 <- getResult2 "inputs/input13.txt"
    print result2
    -- Solution 1: 33780
    -- SOlution 2: 23479

-- Part 1
isHoriReflection :: [[Char]] -> Int -> Bool
isHoriReflection _ 0  = False
isHoriReflection [] _ = False
isHoriReflection rows divideInd 
    | divideInd == length rows = False
    | divideInd <= length rows `div` 2 = reverse (take divideInd rows) == take divideInd (drop divideInd rows)
    | otherwise = take (length (drop divideInd rows)) (drop (length rows - 2*length (drop divideInd rows)) rows) == reverse(drop divideInd rows)

getSummaryNumber :: [[Char]] -> Int
getSummaryNumber lines = getSummaryNumberHori lines (length lines) 100 + getSummaryNumberHori (transpose lines) (length (transpose lines)) 1
    where
        getSummaryNumberHori :: [[Char]] -> Int -> Int -> Int
        getSummaryNumberHori _ 0 _ = 0
        getSummaryNumberHori rows divideInd muliplier
            | isHoriReflection rows divideInd = muliplier * divideInd + getSummaryNumberHori rows (divideInd - 1) muliplier
            | otherwise = getSummaryNumberHori rows (divideInd - 1) muliplier

getResult1 :: FilePath -> IO Int
getResult1 filePath = do
    contents <- readFile filePath
    let strLines = lines contents
    let patterns = groupBy (\a b -> not (null a) && not (null b)) strLines
    let summaryNumbers = map getSummaryNumber patterns
    return $ sum summaryNumbers

-- Part 2
countDifferences :: [[Char]] -> [[Char]] -> Int
countDifferences strings1 strings2 = sum $ zipWith (\char1 char2 -> fromEnum (char1 /= char2)) (concat strings1) (concat strings2)

isSmudgeReflection :: [[Char]] -> Int -> Bool
isSmudgeReflection _ 0  = False
isSmudgeReflection [] _ = False
isSmudgeReflection rows divideInd
    | divideInd == length rows = False
    | divideInd <= length rows `div` 2 = countDifferences (reverse (take divideInd rows)) (take divideInd (drop divideInd rows)) == 1
    | otherwise = countDifferences (take (length (drop divideInd rows)) (drop (length rows - 2*length (drop divideInd rows)) rows)) (reverse(drop divideInd rows)) == 1

getSummaryNumber2 :: [[Char]] -> Int
getSummaryNumber2 lines = getSummaryNumberHori lines (length lines) 100 + getSummaryNumberHori (transpose lines) (length (transpose lines)) 1
    where
        getSummaryNumberHori :: [[Char]] -> Int -> Int -> Int
        getSummaryNumberHori _ 0 _ = 0
        getSummaryNumberHori rows divideInd muliplier
            | isSmudgeReflection rows divideInd = muliplier * divideInd + getSummaryNumberHori rows (divideInd - 1) muliplier
            | otherwise = getSummaryNumberHori rows (divideInd - 1) muliplier

getResult2 :: FilePath -> IO Int
getResult2 filePath = do
    contents <- readFile filePath
    let strLines = lines contents
    let patterns = groupBy (\a b -> not (null a) && not (null b)) strLines
    let summaryNumbers = map getSummaryNumber2 patterns
    return $ sum summaryNumbers