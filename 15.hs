import Data.List ( elemIndex )
import Data.Maybe ( fromJust, isJust )
import Data.HashMap ( Map, lookup, insert, empty )

f :: Int -> [Int] -> Int
f rounds xs = last $ foldr helper xs [length xs..rounds-1]
    where
        helper n xs = playGameRound xs

playGameRound :: [Int] -> [Int]
playGameRound xs = xs ++ [turnsSincePrev (last xs) xs]
turnsSincePrev :: Int -> [Int] -> Int
turnsSincePrev n xs | n `elem` init xs = 1 + fromJust (elemIndex n $ tail $ reverse xs)
                    | otherwise = 0

g :: Int -> [Int] -> Int
g rounds xs = (\(l,old,new) -> l) $ getGameTrace' rounds xs

getGameTrace' :: Int -> [Int] -> (Int, Map Int Int, Map Int Int)
getGameTrace' rounds xs = foldr playGameRound' (getGameStart' xs) [rounds,rounds-1..length xs+1]
getGameStart' :: [Int] -> (Int, Map Int Int, Map Int Int)
getGameStart' xs = (last xs, helper 2, helper 1) :: (Int,Map Int Int,Map Int Int)
    where
        helper offset = foldr (uncurry insert) empty [(xs!!i,i+1)|i<-[0..length xs - offset]]
playGameRound' :: Int -> (Int,Map Int Int,Map Int Int) -> (Int,Map Int Int,Map Int Int)
playGameRound' round (l,old,new) = (newNum,new,Data.HashMap.insert newNum round new)
    where
        newNum :: Int
        newNum = turnsSincePrev' l round old
turnsSincePrev' :: Int -> Int -> Map Int Int -> Int
turnsSincePrev' n round xs | isJust (Data.HashMap.lookup n xs) = round - fromJust (Data.HashMap.lookup n xs) - 1
                           | otherwise = 0

testData :: [Int]
testData = [0,3,6]

main = do
    contents <- readFile "input/15.txt"
    let ss = lines contents
        xs = map read ss :: [Int]
    print ("#1: " ++ show (f 2020 xs))
    print ("#2: " ++ show (g 30000000 xs))