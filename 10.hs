f :: [Int] -> Int
f xs = (\(y1,_,y3) -> y1*y3) (getOneTwoThreeJumps (1,1,1) $ getJumpSizes trace)
    where
        trace :: [Int]
        trace = snd $ findTrace (0,[]) xs

getOneTwoThreeJumps :: (Int,Int,Int) -> [Int] -> (Int,Int,Int)
getOneTwoThreeJumps ys [] = ys
getOneTwoThreeJumps ys [_] = ys
getOneTwoThreeJumps (y1,y2,y3) (x:xs) | x == 1 = getOneTwoThreeJumps (y1+1,y2,y3) xs
                                      | x == 2 = getOneTwoThreeJumps (y1,y2+1,y3) xs
                                      | x == 3 = getOneTwoThreeJumps (y1,y2,y3+1) xs
getJumpSizes :: [Int] -> [Int]
getJumpSizes [] = []
getJumpSizes [_] = []
getJumpSizes (x:x':xs) = x'-x : getJumpSizes (x':xs)
findTrace :: (Int,[Int]) -> [Int] -> (Int,[Int])
findTrace (y,ys) [] = (y,ys)
findTrace (y,ys) xs | nextAdapter /= 0 = findTrace (nextAdapter,ys++[nextAdapter]) xs
                    | otherwise = (y,ys)
    where
        nextAdapter :: Int
        nextAdapter = findNextAdapter y xs
findNextAdapter :: Int -> [Int] -> Int
findNextAdapter y xs | y < maximum xs = minimum $ filter (\x -> x >= y+1 && x <= y+3) xs
                     | otherwise = 0

testData1 :: [Int]
testData1 = [16,10,15,5,1,11,7,19,6,12,4]
testData2 :: [Int]
testData2 = [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]
prop_test1 :: Bool
prop_test1 = f testData2 == 220

g :: [Int] -> Int
g xs = (2 ^ countOneTwoPairs jumps) * product (map helper $ getOneChains jumps)
    where
        jumps :: [Int]
        jumps = getJumpSizes $ snd $ findTrace (0,[]) xs
        helper :: Int -> Int
        helper 0 = 1
        helper 1 = 1
        helper 2 = 2
        helper x | x < 0 = 0
                 | otherwise = helper (x-1) + helper (x-2) + helper (x-3)       

getOneChains :: [Int] -> [Int]
getOneChains = helper . map scrubNotOne
    where
        scrubNotOne :: Int -> Int
        scrubNotOne 1 = 1
        scrubNotOne _ = 0
        helper :: [Int] -> [Int]
        helper [] = []
        helper [x] | x > 0 = [x]
                   | otherwise = []
        helper (x:x':xs) | x > 0 && x' == 1 = helper (x+1:xs)
                         | x > 0 = x : helper xs
                         | otherwise = helper (x':xs)
countOneTwoPairs :: [Int] -> Int
countOneTwoPairs [] = 0
countOneTwoPairs [_] = 0
countOneTwoPairs (x:x':xs) | ((x==1) && (x'==2)) || ((x==2) && (x'==1)) = 1 + countOneTwoPairs (x':xs)
                           | otherwise = countOneTwoPairs (x':xs)

prop_test2 :: Bool
prop_test2 = g testData1 == 8 && g testData2 == 19208

main = do
    contents <- readFile "input/10.txt"
    let ss = lines contents
        xs = map read ss :: [Int]
    print ("#1: " ++ show (f xs))
    print ("#2: " ++ show (g xs))