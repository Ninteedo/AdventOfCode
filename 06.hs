import Data.List (nub)

f :: [String] -> Int
f = sum . map (length . nub . concat) . splitByGroup

splitByGroup :: [String] -> [[String]]
splitByGroup [] = []
splitByGroup ss | null (head ss) && length ss > 1 = helper $ tail ss
                | null (head ss) = []
                | otherwise = helper ss
    where
        helper :: [String] -> [[String]]
        helper ss = takeWhile (not . null) ss : splitByGroup (dropWhile (not . null) ss)

g :: [String] -> Int
g = sum . map (length . appearsAll) . splitByGroup
    where
        appearsBoth :: Eq a => [a] -> [a] -> [a]
        appearsBoth xs = filter (`elem` xs)
        appearsAll :: Eq a => [[a]] -> [a]
        appearsAll [] = []
        appearsAll (xs:xss) = foldr appearsBoth xs xss

main = do
    contents <- readFile "input/06.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))