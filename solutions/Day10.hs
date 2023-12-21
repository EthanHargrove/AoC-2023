main :: IO ()
main = do
    result1 <- getResult1 "inputs/input10.txt"
    print result1
    -- result2 <- getResult2 "inputs/input9.txt"
    -- print result2
    -- Solution 1: 1581679977
    -- SOlution 2: 889

-- Part 1
getResult1 :: FilePath -> IO Int
getResult1 filePath = do
    contents <- readFile filePath
    let strLines = lines contents
    return 9

-- Part 2