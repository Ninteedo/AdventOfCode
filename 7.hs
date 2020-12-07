import Data.List ( isPrefixOf )
import Split (split)
import Data.Char (isDigit)

type BagName = String
type Bag = (BagName,[BagName])
type BagQuant = (BagName,Int)
type Bag' = (BagName,[BagQuant])

f :: [String] -> Int
f xss = length (filter (`searchGold` bagList) bagList) - 1
    where
        bagList :: [Bag]
        bagList = map getBag xss
        searchGold :: Bag -> [Bag] -> Bool
        searchGold (name,others) bagList | name == targetBag = True
                                         | otherwise = any (\other -> other /= noOtherBag && searchGold (other `findBag` bagList) bagList) others

findBag  :: BagName -> [Bag] -> Bag
findBag  name []     = error ("Couldn't find bag " ++ name ++ " in bag list.")
findBag  name (b:bs) | name == fst b = b
                     | otherwise = findBag name bs
findBag' :: BagName -> [Bag'] -> Bag'
findBag' name []     = error ("Couldn't find bag " ++ name ++ " in bag list.")
findBag' name (b:bs) | name == fst b = b
                     | otherwise = findBag' name bs

getBag :: String -> Bag
getBag xs = (\(name,others) -> (name, map fst others)) (getBagQuant xs)
getBagQuant :: String -> Bag'
getBagQuant xs = (getOuterBag xs, getInnerBags xs)
    where
        getOuterBag :: String -> BagName
        getOuterBag = concat . head . split "bags" . map normalizeBagStr . words
        getInnerBags :: String -> [BagQuant]
        getInnerBags = map splitQuant . tail . map (concat . dropWhile (== "contain")) . split "bags" . map normalizeBagStr . init . words
        normalizeBagStr :: String -> String
        normalizeBagStr xs | "bag" `isPrefixOf` xs = "bags"
                           | otherwise = xs
        splitQuant :: String -> (BagName,Int)
        splitQuant xs | not $ null (takeWhile isDigit xs) = (dropWhile isDigit xs, read (takeWhile isDigit xs))
                      | otherwise = (xs,0)

targetBag :: String
targetBag = "shinygold"
noOtherBag :: String
noOtherBag = "noother"

g :: [String] -> Int
g xs = countBags (targetBag `findBag'` bagList)
    where
        bagList :: [Bag']
        bagList = map getBagQuant xs
        countBags :: Bag' -> Int
        countBags (_,others) = sum $ map helper others
            where
                helper :: BagQuant -> Int
                helper (name,quant) | quant == 0 = 0
                                    | otherwise = quant + quant * countBags (name `findBag'` bagList)

test1data :: [String]
test1data = ["light red bags contain 1 bright white bag, 2 muted yellow bags.",
            "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
            "bright white bags contain 1 shiny gold bag.",
            "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
            "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
            "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
            "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
            "faded blue bags contain no other bags.",
            "dotted black bags contain no other bags."]
test1 :: Int
test1 = g test1data
test2data :: [String]
test2data = ["shiny gold bags contain 2 dark red bags.",
            "dark red bags contain 2 dark orange bags.",
            "dark orange bags contain 2 dark yellow bags.",
            "dark yellow bags contain 2 dark green bags.",
            "dark green bags contain 2 dark blue bags.",
            "dark blue bags contain 2 dark violet bags.",
            "dark violet bags contain no other bags."]
test2 :: Int
test2 = g test2data

prop_g :: Bool
prop_g = test1 == 32 && test2 == 126

main = do
    contents <- readFile "input/7.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))