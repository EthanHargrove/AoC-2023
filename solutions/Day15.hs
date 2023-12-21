import Data.List
import Data.Char
import qualified Data.Map as Map

main :: IO ()
main = do
    result1 <- getResult1 "inputs/input15.txt"
    print result1
    result2 <- getResult2 "inputs/input151.txt"
    print result2
    -- Solution 1: 516804
    -- SOlution 2: 23479

-- Part 1
commaToSpace :: [Char] -> [Char]
commaToSpace [] = []
commaToSpace (x:xs)
    | x == ',' = ' ' : commaToSpace xs
    | otherwise = x : commaToSpace xs

getHashValue :: [Char] -> Int
getHashValue [] = 0
getHashValue charList = getHashValueAux charList 0
    where
        getHashValueAux :: [Char] -> Int -> Int
        getHashValueAux [] currVal = currVal
        getHashValueAux (x:xs) currVal = getHashValueAux xs (17 * (currVal + ord x)) `mod` 256


getResult1 :: FilePath -> IO Int
getResult1 filePath = do
    contents <- readFile filePath
    let steps = words (commaToSpace contents)
    return $ sum $ map getHashValue steps

-- Part 2
data Lens = Lens {label :: String, focalLength :: Int} deriving Show

boxContents :: Map.Map Int [Lens]
boxContents = Map.fromList $ zip [0..255] (replicate 256 [])

updateContents :: Map.Map Int [Lens] -> [Char] -> Map.Map Int [Lens]
updateContents contents step
    | last step == '-' = Map.adjust (filter (\lens -> label lens /= take 2 step)) (getHashValue (fst (break (== '-') step))) contents
    | otherwise = Map.adjust (++ [Lens (fst (break (== '=') step)) (digitToInt (last step))]) (getHashValue (fst (break (== '=') step))) contents

getFocusingPower :: Int -> [Lens] -> Int
getFocusingPower _ [] = 0
getFocusingPower boxNum (x:xs) = getFocusingPowerAux (boxNum+1) (x:xs) 1
    where
        getFocusingPowerAux :: Int -> [Lens] -> Int -> Int
        getFocusingPowerAux _ [] _ = 0
        getFocusingPowerAux boxNum (x:xs) slotNum = boxNum * slotNum * focalLength x + getFocusingPowerAux boxNum xs (slotNum + 1)

getResult2 :: FilePath -> IO Int
getResult2 filePath = do
    contents <- readFile filePath
    let steps = words (commaToSpace contents)
    let boxContents = foldl updateContents (Map.fromList $ zip [0..255] (replicate 256 [])) steps
    print boxContents
    let focusingPower = sum $ Map.elems $ Map.mapWithKey getFocusingPower boxContents
    return focusingPower