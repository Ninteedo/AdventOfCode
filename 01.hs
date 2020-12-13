f :: [Int] -> Int
f xs = head [ xs!!i * xs!!j |
              i <- [0..length xs - 1], j <- [0..length xs - 1],
              i /= j,
              xs!!i + xs!!j == 2020 ]

g :: [Int] -> Int
g xs = head [ xs!!i * xs!!j * xs!!k |
              i <- [0..length xs - 1], j <- [0..length xs - 1], k <- [0..length xs - 1],
              i /= j && j /= k,
              xs!!i + xs!!j + xs!!k == 2020 ]

main = do
    contents <- readFile "input/01.txt"
    let ss = lines contents
        xs :: [Int]
        xs = map read ss
    print ("#1: " ++ show (f xs))
    print ("#2: " ++ show (g xs))