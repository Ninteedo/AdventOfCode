import Data.Char ( isDigit )

f :: [String] -> Int
f = length . validPassports
    
validPassports :: [String] -> [String]
validPassports sss = filter validPassport (splitPassports sss)
    where
        splitPassports :: [String] -> [String]
        splitPassports [] = []
        splitPassports [ss] = [ss]
        splitPassports (ss:sss) | null ss = [] : splitPassports sss
                                | otherwise = (ss ++ " " ++ head (splitPassports sss)) : tail (splitPassports sss)

        findKeys :: String -> [String]
        findKeys ss = [ takeWhile (/= ':') w | w <- words ss ]
        requiredKeys :: [String]
        requiredKeys = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
        validPassport :: String -> Bool
        validPassport ss = all (`elem` findKeys ss) requiredKeys

g :: [String] -> Int
g = length . extraValidPassports

extraValidPassports :: [String] -> [String]
extraValidPassports sss = filter moreValid (validPassports sss)
    where
        moreValid :: String -> Bool
        moreValid ss = all (validField . toKeyValPair) (words ss)
        toKeyValPair :: String -> (String,String)
        toKeyValPair ss = (takeWhile (/= ':') ss, tail $ dropWhile (/= ':') ss)
        validField :: (String,String) -> Bool
        validField ("byr",val) = read val >= 1920 && read val <= 2002
        validField ("iyr",val) = read val >= 2010 && read val <= 2020
        validField ("eyr",val) = read val >= 2020 && read val <= 2030
        validField ("hgt",val) | dropWhile isDigit val == "cm" = read (takeWhile isDigit val) >= 150 && read (takeWhile isDigit val) <= 193
                               | dropWhile isDigit val == "in" = read (takeWhile isDigit val) >= 59 && read (takeWhile isDigit val) <= 76
                               | otherwise = False
        validField ("hcl",val) | head val == '#' = length (tail val) == 6 && all isHex (tail val)
                               | otherwise = False
        validField ("ecl",val) = val `elem` eyeColours
        validField ("pid",val) = length val == 9 && all isDigit val
        validField ("cid",_)   = True
        isHex :: Char -> Bool
        isHex c = isDigit c || (c >= 'a' && c <= 'f')
        eyeColours :: [String]
        eyeColours = ["amb","blu","brn","gry","grn","hzl","oth"]
    
main = do
    contents <- readFile "input/04.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))