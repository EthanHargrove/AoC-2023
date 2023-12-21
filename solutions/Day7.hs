import Data.List
import Data.Maybe
import Data.Ord

main :: IO ()
main = do
    result1 <- getResult1 "inputs/input7.txt"
    print result1
    result2 <- getResult2 "inputs/input7.txt"
    print result2
    -- Solution 1: 250254244
    -- SOlution 2: 250087440

-- Part 1
cardOrder = ['2','3','4','5','6','7','8','9','T','J','Q','K','A']
getCardPoint :: Char -> Int
getCardPoint card = fromJust (elemIndex card cardOrder) + 2

tieBreakerScore :: [Char] -> Int
tieBreakerScore = tieBreakerAux 4
    where
        tieBreakerAux _ [] = 0
        tieBreakerAux n (x:xs) = getCardPoint x * 13^n + tieBreakerAux (n-1) xs

cardCounts :: [Char] -> [Int]
cardCounts = map length . group . sort

handScore :: [Char] -> Int
handScore hand
    | maximum (cardCounts hand) == 5    = 6 * 433174 + tieBreakerScore hand
    | maximum (cardCounts hand) == 4    = 5 * 433174 + tieBreakerScore hand
    | sort (cardCounts hand) == [2,3]   = 4 * 433174 + tieBreakerScore hand
    | maximum (cardCounts hand) == 3    = 3 * 433174 + tieBreakerScore hand
    | sort (cardCounts hand) == [1,2,2] = 2 * 433174 + tieBreakerScore hand
    | maximum (cardCounts hand) == 2    = 1 * 433174 + tieBreakerScore hand
    | otherwise = tieBreakerScore hand

compareHands :: [String] -> [String] -> Ordering
compareHands = comparing (handScore . head)

getTotalWinnings :: [[String]] -> Int
getTotalWinnings = totalWinningsAux 1
    where 
        totalWinningsAux _ [] = 0
        totalWinningsAux n (x:xs) = n * read (last x) + totalWinningsAux (n+1) xs

getResult1 :: FilePath -> IO Int
getResult1 filePath = do
    contents <- readFile filePath
    let handsAndBids = map words (lines contents)
    let sortedHands = sortBy compareHands handsAndBids
    let totalWinnings = getTotalWinnings sortedHands
    return totalWinnings

-- Part 2
cardOrder2 = ['J','2','3','4','5','6','7','8','9','T','Q','K','A']

getCardPoint2 :: Char -> Int
getCardPoint2 card = fromJust (elemIndex card cardOrder2) + 2

tieBreakerScore2 :: [Char] -> Int
tieBreakerScore2 = tieBreakerAux2 4
    where
        tieBreakerAux2 _ [] = 0
        tieBreakerAux2 n (x:xs) = getCardPoint2 x * 13^n + tieBreakerAux2 (n-1) xs

numJokers :: [Char] -> Int
numJokers = length . filter (== 'J')

cardCounts2 :: [Char] -> [Int]
cardCounts2 = map length . group . sort . filter (/= 'J')

handScore2 :: [Char] -> Int
handScore2 hand
    | null (cardCounts2 hand) = 6 * 433174 + tieBreakerScore2 hand
    | maximum (cardCounts2 hand) + numJokers hand == 5 = 6 * 433174 + tieBreakerScore2 hand
    | maximum (cardCounts2 hand) + numJokers hand == 4 = 5 * 433174 + tieBreakerScore2 hand
    | cardCounts2 hand == [2,2] && numJokers hand == 1 = 4 * 433174 + tieBreakerScore2 hand
    | sort (cardCounts2 hand) == [2,3]                 = 4 * 433174 + tieBreakerScore2 hand
    | maximum (cardCounts2 hand) + numJokers hand == 3 = 3 * 433174 + tieBreakerScore2 hand
    | sort (cardCounts2 hand) == [1,2,2]               = 2 * 433174 + tieBreakerScore2 hand
    | maximum (cardCounts2 hand) + numJokers hand == 2 = 1 * 433174 + tieBreakerScore2 hand
    | otherwise = tieBreakerScore2 hand

compareHands2 :: [String] -> [String] -> Ordering
compareHands2 = comparing (handScore2 . head)

getResult2 :: FilePath -> IO Int
getResult2 filePath = do
    contents <- readFile filePath
    let handsAndBids = map words (lines contents)
    let sortedHands = sortBy compareHands2 handsAndBids
    let totalWinnings = getTotalWinnings sortedHands
    return totalWinnings