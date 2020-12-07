module Split (split) where

split :: Eq a => a -> [a] -> [[a]]
split _ []  = []
split _ [x] = [[x]]
split d (x:xs) | d == x = [] : split d xs
               | otherwise = (x : head (split d xs)) : tail (split d xs)