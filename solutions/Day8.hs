import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
    result1 <- getResult1 "inputs/input8.txt"
    print result1
    result2 <- getResult2 "inputs/input8.txt"
    print result2
    -- Solution 1: 12599
    -- SOlution 2: 8245452805243

-- Part 1
type NodeMap = Map.Map [Char] [String]

lineToMap :: NodeMap -> [Char] -> NodeMap
lineToMap nodeMap line = Map.insert (take 3 line) [take 3 (drop 7 line), take 3 (drop 12 line)] nodeMap

numSteps :: NodeMap -> [Char] -> Int
numSteps = numStepsAux 0 "AAA"
    where 
        numStepsAux :: Int -> [Char] -> NodeMap -> [Char] -> Int
        numStepsAux step key nodeMap instructions
            | key == "ZZZ" = step
            | instructions !! max 0 (step `mod` length instructions) == 'L' = case Map.lookup key nodeMap of
                Just values -> numStepsAux (step+1) (head values) nodeMap instructions
            | instructions !! max 0 (step `mod` length instructions) == 'R' = case Map.lookup key nodeMap of
                Just values -> numStepsAux (step+1) (values !! 1) nodeMap instructions

getResult1 :: FilePath -> IO Int
getResult1 filePath = do
    contents <- readFile filePath
    let strLines = lines contents
    let instructions = head strLines
    let networkStr = drop 2 strLines
    let nodeMap = foldl lineToMap Map.empty networkStr
    let steps = numSteps nodeMap instructions
    return steps

-- Part 2
getKeysEndInA :: NodeMap -> [[Char]]
getKeysEndInA nodeMap = filter (\key -> last key == 'A') (Map.keys nodeMap)

numSteps2 :: NodeMap -> [Char] -> [Char] -> Int
numSteps2 = numSteps2Aux 0
    where 
        numSteps2Aux :: Int -> NodeMap -> [Char] -> [Char] -> Int
        numSteps2Aux step nodeMap instructions key
            | last key == 'Z' = step
            | instructions !! max 0 (step `mod` length instructions) == 'L' = case Map.lookup key nodeMap of
                Just values -> numSteps2Aux (step+1) nodeMap instructions (head values)
            | instructions !! max 0 (step `mod` length instructions) == 'R' = case Map.lookup key nodeMap of
                Just values -> numSteps2Aux (step+1) nodeMap instructions (values !! 1)


getResult2 :: FilePath -> IO Int
getResult2 filePath = do
    contents <- readFile filePath
    let strLines = lines contents
    let instructions = head strLines
    let networkStr = drop 2 strLines
    let nodeMap = foldl lineToMap Map.empty networkStr
    let keysEndInA = getKeysEndInA nodeMap
    let steps = map (numSteps2 nodeMap instructions) keysEndInA
    let result = foldl lcm 1 steps
    return result
