import Split ( split )
import Data.Either ( lefts, rights )
import Data.HashMap as HM ( Map, insert, (!), empty, member )
import Text.Regex ( Regex, mkRegex, matchRegex )
import Data.Maybe ( isJust )

type Rules = Map Int Rule
data Rule = Match Char | Subrules [Int] | Disjunct Rule Rule | Star Rule | Chain Rule Rule deriving Show

f :: [String] -> Int
f ss = length $ filter (\i -> checkImageMatchRule i (getRule 0 $ rules ss) $ rules ss) $ images ss

rules :: [String] -> Rules
rules = foldr (uncurry insert) empty . lefts . map readLine
images :: [String] -> [String]
images = filter (not.null) . rights . map readLine

readLine :: String -> Either (Int,Rule) String
readLine line | ':' `elem` line = Left (readRule line)
              | otherwise       = Right line
readRule :: String -> (Int,Rule)
readRule str = (ruleId, readLookup $ drop 1 $ dropWhile (/=' ') str)
    where
        ruleId :: Int
        ruleId = read $ takeWhile (/=':') str
        readLookup :: String -> Rule
        readLookup ('"':c:['"']) = Match c
        readLookup str | '|' `elem` str = Disjunct (readLookup $ takeWhile (/='|') str) (readLookup $ drop 1 $ dropWhile (/='|') str)
                       | ruleId `elem` readSubrules str = Chain (Star (Subrules (takeWhile (/=ruleId) $ readSubrules str))) (Subrules (drop 1 $ dropWhile (/=ruleId) $ readSubrules str))
                       | otherwise = Subrules (readSubrules str)
        readSubrules :: String -> [Int]
        readSubrules = map read . filter (not.null) . split " "

checkImageMatchRule :: String -> Rule -> Rules -> Bool
checkImageMatchRule xs r rs = testRegex xs (mkRegex ("^("++ toRegex r rs ++")$"))

allSubrulesMatch :: String -> [Int] -> Rules -> Bool
allSubrulesMatch xs [m] rs = checkImageMatchRule xs (getRule m rs) rs
allSubrulesMatch xs (m:ms) rs = any (\l -> allSubrulesMatch (drop l xs) ms rs) possibleLengths
    where
        possibleLengths :: [Int]
        possibleLengths = [ l | l <- [1..length xs - 1], checkImageMatchRule (take l xs) (getRule m rs) rs]
getRule :: Int -> Rules -> Rule
getRule n rs | n `member` rs = rs ! n
             | otherwise = error ("rule number " ++ show n ++ " does not exist")

toRegex :: Rule -> Rules -> String
toRegex (Match c) rs = [c]
toRegex (Subrules ms) rs = concatMap ((`toRegex` rs) . (`getRule` rs)) ms
toRegex (Disjunct ms ms') rs = "(" ++ toRegex ms rs ++ "|" ++ toRegex ms' rs ++ ")"
toRegex (Star ms) rs = "(" ++ toRegex ms rs ++ ")*"
toRegex (Chain ms ms') rs = toRegex ms rs ++ toRegex ms' rs
testRegex :: String -> Regex -> Bool
testRegex input regex = isJust output
    where
        output = matchRegex regex input

testData :: [String]
testData = ["0: 4 1 5",
            "1: 2 3 | 3 2",
            "2: 4 4 | 5 5",
            "3: 4 5 | 5 4",
            "4: \"a\"",
            "5: \"b\"",
            "",
            "ababbb",
            "bababa",
            "abbbab",
            "aaabbb",
            "aaaabbb"]

g :: [String] -> Int
g ss = length $ filter (\i -> checkImageMatchRule i (getRule 0 $ rules' ss) $ rules' ss) $ images ss

rules' :: [String] -> Rules
rules' ss = replaceNewRules $ rules ss

{-replaceNewRules :: [String] -> [String]
replaceNewRules [] = []
replaceNewRules (x:xs) | take 2 x == "8:"  = "8: 42 | 42 8" : replaceNewRules xs
                       | take 3 x == "11:" = "11: 42 31 | 42 11 31" : replaceNewRules xs
                       | otherwise = x : replaceNewRules xs-}

replaceNewRules :: Rules -> Rules
replaceNewRules = insert 8 (Disjunct (Subrules [42]) (Chain (Star (Subrules [42])) (Subrules []))) . insert 11 (Disjunct (Subrules [42,31]) (Chain (Star (Disjunct (Subrules [42,31]) (Subrules [42]))) (Subrules [31])))

main = do
    contents <- readFile "input/19.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))