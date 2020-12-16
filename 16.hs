import Split ( split )

type Field = (String,(Int,Int),(Int,Int))

f :: [String] -> Int
f ss = sum $ map (sum . (`filterInvalidValues` fields)) nearbyTickets
    where
        (fieldsStr,_:yourTicketStr:_) = break (=="your ticket:") ss
        (_,_:nearbyTicketsStr) = break (=="nearby tickets:") ss
        fields = ticketFields fieldsStr :: [Field]
        yourTicket = ticketValues yourTicketStr :: [Int]
        nearbyTickets = map ticketValues nearbyTicketsStr :: [[Int]]

filterInvalidValues :: [Int] -> [Field] -> [Int]
filterInvalidValues xs fs = filter (not . helper) xs
    where
        helper x = any (validValue x) fs
ticketValues :: String -> [Int]
ticketValues = map read . split ","
validValue :: Int -> Field -> Bool
validValue n (_,(min1,max1),(min2,max2)) = (n >= min1 && n <= max1) || (n >= min2 && n <= max2)
ticketFields :: [String] -> [Field]
ticketFields = map vals . filter (not . null)
    where
        valStr :: String -> String
        valStr = drop 2 . dropWhile (/=':')
        vals :: String -> Field
        vals str = (takeWhile (/=':') str,helper $ takeWhile (/=' ') $ valStr str, helper $ drop 4 $ dropWhile (/=' ') $ valStr str)
            where
                helper :: String -> (Int,Int)
                helper str = (read $ takeWhile (/='-') str, read $ drop 1 $ dropWhile (/='-') str)

testData :: [String]
testData = ["class: 1-3 or 5-7",
            "row: 6-11 or 33-44",
            "seat: 13-40 or 45-50",
            "",
            "your ticket:",
            "7,1,14",
            "",
            "nearby tickets:",
            "7,3,47",
            "40,4,50",
            "55,2,20",
            "38,6,12"]

g :: [String] -> Int
g ss = product (lookupFields fieldsNeeded (getReducedPossiblities nearbyTickets fields) yourTicket)
    where
        (fieldsStr,_:yourTicketStr:_) = break (=="your ticket:") ss
        (_,_:nearbyTicketsStr) = break (=="nearby tickets:") ss
        fields = ticketFields fieldsStr :: [Field]
        yourTicket = ticketValues yourTicketStr :: [Int]
        nearbyTickets = map ticketValues nearbyTicketsStr :: [[Int]]
        fieldsNeeded = map ("departure "++) ["location","station","platform","track","date","time"] :: [String]

getReducedPossiblities :: [[Int]] -> [Field] -> [Field]
getReducedPossiblities tickets fields = map the $ many reducePossibilities $ fieldPossibilities (filterInvalidTickets tickets fields) fields
    where
        reducePossibilities :: [[Field]] -> [[Field]]
        reducePossibilities fss = many (map (\fs -> reduceCol fs fs fss)) fss
            where
                reduceCol :: [Field] -> [Field] -> [[Field]] -> [Field]
                reduceCol [] orig _ = orig
                reduceCol (f:fs) orig fss | appearances f fss == 1 = [f]
                                        | otherwise = reduceCol fs orig fss
                appearances :: Field -> [[Field]] -> Int
                appearances f fss = length (filter (f `elem`) fss)
        fieldPossibilities :: [[Int]] -> [Field] -> [[Field]]
        fieldPossibilities xss fs = [ [ f | f <- fs, all (\xs -> validValue (xs!!i) f) xss ] | i <- [0..length (head xss) - 1] ]
        filterInvalidTickets :: [[Int]] -> [Field] -> [[Int]]
        filterInvalidTickets xss fs = filter (\xs -> null $ filterInvalidValues xs fs) xss

lookupFields :: [String] -> [Field] -> [Int] -> [Int]
lookupFields fns fs xs = map (\fn -> lookupField fn fs xs) fns
lookupField :: String -> [Field] -> [Int] -> Int
lookupField fn fs xs = snd $ head $ filter helper (zip fs xs)
    where
        helper :: (Field,Int) -> Bool
        helper = \((n,_,_),_) -> n == fn

many :: Eq a => (a -> a) -> a -> a
many f x | x == f x  = x
         | otherwise = many f (f x)
the :: [a] -> a
the [x] = x

main = do
    contents <- readFile "input/16.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))