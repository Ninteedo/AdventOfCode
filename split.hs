module Split (split, splitEnd) where

split :: Eq a => [a] -> [a] -> [[a]]
split d xs = dropDelimiters d $ stdSplit d xs
splitEnd :: Eq a => [a] -> [a] -> [[a]]
splitEnd d xs = dropDelimitersEnd d $ stdSplit d xs

stdSplit :: Eq a => [a] -> [a] -> [[a]]
stdSplit _ []  = []
stdSplit _ [x] = [[x]]
stdSplit d xs | d == take (length d) xs = take (length d) xs : stdSplit d (drop (length d) xs)
                | otherwise = (head xs : head (stdSplit d $ tail xs)) : tail (stdSplit d $ tail xs)

dropDelimiters :: Eq a => [a] -> [[a]] -> [[a]]
dropDelimiters d xss = init (map (\xs -> take (length xs - length d) xs) xss) ++ [last xss]
dropDelimitersEnd :: Eq a => [a] -> [[a]] -> [[a]]
dropDelimitersEnd d xss | d == drop (length (last xss) - length d) (last xss) = init $ dropDelimiters d (xss ++ [last xss])
                        | otherwise = dropDelimiters d xss