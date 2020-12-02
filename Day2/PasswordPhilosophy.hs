f :: [String] -> Int
f ss = length $ filter checkValid ss
    where
        checkValid :: String -> Bool
        checkValid line = watchCharCount >= read lowLimit && watchCharCount <= read highLimit
            where
                (lowLimit,_:rest) = break (== '-') line
                (highLimit,_:watchChar:_:_:password) = break (== ' ') rest
                watchCharCount = length $ filter (==watchChar) password

g :: [String] -> Int
g ss = length $ filter checkValid ss
    where
        checkValid :: String -> Bool
        checkValid line = (p && not q) || (not p && q)
            where
                (firstI,_:rest) = break (== '-') line
                (secondI,_:watchChar:_:_:password) = break (== ' ') rest
                firstC  | read firstI  > length password = ' '
                        | otherwise = password !! (read firstI  - 1)
                secondC | read secondI > length password = ' '
                        | otherwise = password !! (read secondI - 1)
                p = firstC  == watchChar
                q = secondC == watchChar

main = do
    contents <- readFile "Day2/input.txt"
    let ss = lines contents
    print (f ss)
    print (g ss)