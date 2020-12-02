f :: [String] -> Int
f = length . filter checkValid
    where
        checkValid :: String -> Bool
        checkValid line = watchCharCount >= read lowLimit && watchCharCount <= read highLimit
            where
                (lowLimit,'-':rest) = break (== '-') line
                (highLimit,' ':watchChar:':':' ':password) = break (== ' ') rest
                watchCharCount = length $ filter (==watchChar) password

g :: [String] -> Int
g = length . filter checkValid
    where
        checkValid :: String -> Bool
        checkValid line = (p && not q) || (not p && q)  --xor
            where
                (firstI,'-':rest) = break (== '-') line
                (secondI,' ':watchChar:':':' ':password) = break (== ' ') rest
                firstC  | read firstI  > length password = ' '
                        | otherwise = password !! (read firstI  - 1)
                secondC | read secondI > length password = ' '
                        | otherwise = password !! (read secondI - 1)
                p = firstC  == watchChar
                q = secondC == watchChar

main = do
    contents <- readFile "input/2.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))