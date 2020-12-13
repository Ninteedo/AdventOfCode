f :: [String] -> (Int, Int) -> Int
f ss (slope1,slope2) = length $ filter (=='#') [ ss !! rowNum !! colNum rowNum | rowNum <- [slope2,slope2*2..length ss - 1] ]
    where
        colNum :: Int -> Int
        colNum rowNum = ((rowNum `div` slope2) * slope1) `mod` length (head ss)

g :: [String] -> Int
g ss = product $ map (f ss) directions
    where
        directions :: [(Int,Int)]
        directions = [(1,1),(3,1),(5,1),(7,1),(1,2)]

main = do
    contents <- readFile "input/03.txt"
    let ss = lines contents
        fDirection = (3,1)
    print ("#1: " ++ show (f ss fDirection))
    print ("#2: " ++ show (g ss))