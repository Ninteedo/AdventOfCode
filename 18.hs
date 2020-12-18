data Expr = Val Int | Add Expr Expr | Mult Expr Expr deriving Show

toExpr :: Bool -> String -> Expr
toExpr addP xs | not containsOp = Val  (read xs)
               | op == '+'      = Add  (toExpr addP before) (toExpr addP after)
               | op == '*'      = Mult (toExpr addP before) (toExpr addP after)
               | isBracketed    = toExpr addP (init $ tail xs)
               | op == ' '      = toExpr addP before
    where
        isBracketed = (head xs,last xs) == ('(',')')
        containsOp = any (`elem` xs) "+*"
        (before,op,after) | operatorIndex < length xs = (take operatorIndex xs, xs!!operatorIndex, drop (operatorIndex+1) xs)
                          | otherwise = (xs,' ',"")
        operatorIndex = length xs - 1 - helper 0 0 (reverse xs)
            where
                helper :: Int -> Int -> String -> Int
                helper n i [] = -1
                helper n i (x:xs) | n == 0 && not addP && x `elem` "+*" = i
                                  | n == 0 && x == '*' = i
                                  | n == 0 && '*' `notElem` topLevelOps && x == '+' = i
                                  | x == '(' = helper (n+1) (i+1) xs
                                  | x == ')' = helper (n-1) (i+1) xs
                                  | otherwise = helper n (i+1) xs
        topLevelOps = helper 0 [] xs
            where
                helper :: Int -> [Char] -> String -> [Char]
                helper n os [] = os
                helper n os (x:xs) | n == 0 && x `elem` "+*" = helper n (x:os) xs
                                   | x == '(' = helper (n+1) os xs
                                   | x == ')' = helper (n-1) os xs
                                   | otherwise = helper n os xs

evalExpr :: Expr -> Int
evalExpr (Val x)    = x
evalExpr (Add p q)  = evalExpr p + evalExpr q
evalExpr (Mult p q) = evalExpr p * evalExpr q

f :: [String] -> Int
f = sum . map (evalExpr . toExpr False . filter (/=' '))

g :: [String] -> Int
g = sum . map (evalExpr . toExpr True . filter (/=' '))

main = do
    contents <- readFile "input/18.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))