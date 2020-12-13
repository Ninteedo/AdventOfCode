f :: [Int] -> Int
f xs = head [ xs !! i | i <- [offset..length xs-1], not $ checkValid (xs!!i) (take offset (drop (i-offset) xs)) ]
    where
        offset :: Int
        offset = 25

checkValid :: Int -> [Int] -> Bool
checkValid y xs = y `elem` pairSums xs
    where
        pairSums :: [Int] -> [Int]
        pairSums [] = []
        pairSums (x:xs) = map (x+) xs ++ pairSums xs

g :: [Int] -> Int
g xs = (\ys -> minimum ys + maximum ys) (findSum (f xs) xs)

findSum :: Int -> [Int] -> [Int]
findSum _ [] = []
findSum target (x:xs) | x + sum (strictFind (target-x) xs) == target = x : strictFind (target-x) xs
                      | otherwise = findSum target xs
    where
        strictFind :: Int -> [Int] -> [Int]
        strictFind _ [] = []
        strictFind target (x:xs) | x + sum (strictFind (target-x) xs) == target = x : strictFind (target-x) xs
                                 | otherwise = []

main = do
    contents <- readFile "input/09.txt"
    let ss = lines contents
        xs = map read ss :: [Int]
    print ("#1: " ++ show (f xs))
    print ("#2: " ++ show (g xs))