main = do
  let result = getResult [34, 90, 89, 86] [204, 1713, 1210, 1780]
  putStrLn ("Solution 1: " ++ show result)
  let result2 = getNumWays 34908986 204171312101780
  putStrLn ("Solution 2: " ++ show result2)
  -- Solution 1: 633080
  -- Solution 2: 20048741

getFirstRecord :: Int -> Int -> Int
getFirstRecord time distance = ceiling $ (fromIntegral time - sqrt (fromIntegral (time^2 - 4 * distance))) / 2

getLastRecord :: Int -> Int -> Int
getLastRecord time distance = ceiling $ (fromIntegral time + sqrt (fromIntegral (time^2 - 4 * distance))) / 2

getNumWays :: Int -> Int -> Int
getNumWays time distance = getLastRecord time distance - getFirstRecord time distance

getResult :: [Int] -> [Int] -> Int
getResult times distances = product $ zipWith getNumWays times distances