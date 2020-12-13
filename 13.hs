import Split (split)
import ChineseRemainderTheorem (chineseRemainder)

f :: [String] -> Int
f css = shortestWait * shortestWaitId
    where
        earliest = read $ head css :: Int
        busIds = map read $ filter (/= "x") $ split "," $ last css :: [Int]
        waits = [ (id,id - earliest `mod` id) | id <- busIds ] :: [(Int,Int)]
        shortestWait = minimum $ map snd waits :: Int
        shortestWaitId = fst $ head $ filter (\(id,wait) -> wait == shortestWait) waits :: Int

testData :: [String]
testData = ["939",
            "7,13,x,x,59,x,31,19"]

g :: [String] -> Either String Int
g css = chineseRemainder [ busIds !! i - i | i <- [0..length busIds - 1], busIds !! i >= 0 ] [ busIds !! i | i <- [0..length busIds - 1], busIds !! i >= 0 ]
    where
        busIds = map keepRead $ split "," $ last css :: [Int]
        keepRead :: String -> Int
        keepRead "x" = -1
        keepRead x = read x

main = do
    contents <- readFile "input/13.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))