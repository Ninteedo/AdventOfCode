import Data.Char ( isDigit )
import KeymapTree ( Keymap, toList, set, fromList )

type StateTree = (Mask,Keymap Integer Integer)
type Mask = [Binary]
data Binary = Zero | One | Empty deriving Show

f :: [String] -> Integer
f = sumProcessValues False

g :: [String] -> Integer
g = sumProcessValues True

sumProcessValues :: Bool -> [String] -> Integer
sumProcessValues float = sum . map snd . toList . snd . foldr (processLine float) ([],fromList [(50000,0)]) . reverse

processLine :: Bool -> String -> StateTree -> StateTree
processLine float cs (mask,ms) | instr == "mask " = (readMask val,ms)
                               | not float = (mask,set (read memId) newVal ms)
                               | float     = (mask,foldr (\id -> set id (read val)) ms memIds)
    where
        (instr,'=':' ':val) = break (== '=') cs
        memId = takeWhile isDigit $ dropWhile (not . isDigit) instr :: String
        memIds = fromBinaryF $ applyBitMask mask True $ correctBinaryLength (length mask) $ toBinary $ read memId :: [Integer]
        newVal = fromBinary $ applyBitMask mask False $ correctBinaryLength (length mask) $ toBinary $ read val :: Integer
applyBitMask :: Mask -> Bool -> [Binary] -> [Binary]
applyBitMask m float xs | length m /= length xs = error ("tried to apply mask of differing length to binary number (length " ++ show (length m) ++ " versus " ++ show (length xs))
                        | not float = map (uncurry updateDigit)  (zip m xs)
                        | float     = map (uncurry updateDigitF) (zip m xs)
    where
        updateDigit :: Binary -> Binary -> Binary
        updateDigit Empty bd = bd
        updateDigit md _     = md
        updateDigitF :: Binary -> Binary -> Binary
        updateDigitF Zero bd = bd
        updateDigitF One _   = One
        updateDigitF Empty _ = Empty
correctBinaryLength :: Int -> [Binary] -> [Binary]
correctBinaryLength l xs = replicate (l - length xs) Zero ++ xs
toBinary :: Integer -> [Binary]
toBinary 0 = [Zero]
toBinary 1 = [One]
toBinary n = toBinary (n `div` 2) ++ toBinary (n `mod` 2)
fromBinary :: [Binary] -> Integer
fromBinary = helper . reverse
    where
        helper :: [Binary] -> Integer
        helper [] = 0
        helper (Zero:xs) = 2 * helper xs
        helper (One:xs) = 1 + 2 * helper xs
        helper (Empty:xs) = 2 * helper xs
fromBinaryF :: [Binary] -> [Integer]
fromBinaryF = helper . reverse
    where
        helper :: [Binary] -> [Integer]
        helper [] = [0]
        helper (Zero:xs) = map (*2) (helper xs)
        helper (One:xs) = map (\x -> x*2+1) (helper xs)
        helper (Empty:xs) = helper (Zero:xs) ++ helper (One:xs)
readMask :: String -> [Binary]
readMask [] = []
readMask ('X':cs) = Empty : readMask cs
readMask ('0':cs) = Zero  : readMask cs
readMask ('1':cs) = One   : readMask cs

testData :: [String]
testData = ["mask = 000000000000000000000000000000X1001X",
            "mem[42] = 100",
            "mask = 00000000000000000000000000000000X0XX",
            "mem[26] = 1"]

main = do
    contents <- readFile "input/14.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))