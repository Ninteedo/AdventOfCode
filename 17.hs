import Data.HashMap as HM ( Map, lookup, insert, keys, empty, filter )

type Location = (Int,Int,Int,Int) -- x,y,z,w
type Grid = Map Location Bool

f :: [String] -> Int
f = countAllActive . repeatUpdateAllStates False 6 . textToGrid

isActive :: Maybe Bool -> Bool
isActive Nothing  = False
isActive (Just s) = s
countAllActive :: Grid -> Int
countAllActive = length . HM.filter (==True)

repeatUpdateAllStates :: Bool -> Int -> Grid -> Grid
repeatUpdateAllStates four 0 g = g
repeatUpdateAllStates four x g = repeatUpdateAllStates four (x-1) (updateAllStates four g)
updateAllStates :: Bool -> Grid -> Grid
updateAllStates four g = foldr (uncurry insert . (`updateState` g)) g [ (x',y',z',w') | (x,y,z,w) <- keys g, x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], all (uncurry (==)) [(x,x'),(y,y'),(z,z')] || isActive (HM.lookup (x,y,z,w) g), w' <- [w-1..w+1], four || w == w' ]
updateState :: Location -> Grid -> (Location,Bool)
updateState loc g = (loc,newState)
    where
        newState :: Bool
        newState | currentState = activeNeighbors == 2 || activeNeighbors == 3
                 | otherwise    = activeNeighbors == 3
        currentState = isActive $ HM.lookup loc g :: Bool
        activeNeighbors = countActiveNeighbors loc g :: Int
        countActiveNeighbors :: Location -> Grid -> Int
        countActiveNeighbors (x,y,z,w) g = length [ True | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], w' <- [w-1..w+1], any (/=0) [x'-x,y'-y,z'-z,w'-w], isActive $ HM.lookup (x',y',z',w') g ]

textToGrid :: [String] -> Grid
textToGrid xss = foldr (uncurry insert) empty [ ((i,j,0,0),toState (xss!!i!!j)) | i <- [0..length xss - 1], j <- [0..length (head xss) - 1] ]
    where
        toState :: Char -> Bool
        toState '.' = False
        toState '#' = True
gridToText :: Grid -> [[String]]
gridToText g = [ ("z=" ++ show z ++ ", w=" ++ show w) : [ [ getState (x,y,z,w) | x <- [minX..maxX] ] | y <- [minY..maxY] ] | z <- [minZ..maxZ], w <- [minW..maxW] ]
    where
        getState :: Location -> Char
        getState loc = fromState $ HM.lookup loc g
        fromState :: Maybe Bool -> Char
        fromState Nothing      = '.'
        fromState (Just False) = '.'
        fromState (Just True)  = '#'
        (minX,maxX) = (\xs -> (minimum xs, maximum xs)) (map (\(x,_,_,_) -> x) activeKeys)
        (minY,maxY) = (\ys -> (minimum ys, maximum ys)) (map (\(_,y,_,_) -> y) activeKeys)
        (minZ,maxZ) = (\zs -> (minimum zs, maximum zs)) (map (\(_,_,z,_) -> z) activeKeys)
        (minW,maxW) = (\ws -> (minimum ws, maximum ws)) (map (\(_,_,_,w) -> w) activeKeys)
        activeKeys = keys (HM.filter (==True) g) :: [Location]

testData :: [String]
testData = [".#.",
        "..#",
        "###"]

g :: [String] -> Int
g = countAllActive . repeatUpdateAllStates True 6 . textToGrid

main = do
    contents <- readFile "input/17.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))