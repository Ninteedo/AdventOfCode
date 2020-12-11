f :: [String] -> Int
f = sum . map (length . filter isSeatFull) . many seatingRound

many :: Eq a => (a -> a) -> a -> a
many h x | h x == x  = x
         | otherwise = many h (h x)
isSeatFull :: Char -> Bool
isSeatFull '#' = True
isSeatFull _   = False
isFloor :: Char -> Bool
isFloor '.' = True
isFloor _   = False
seatingRound :: [String] -> [String]
seatingRound css = [ [ newStatus (x,y) css | x <- [0..length (head css)-1] ] | y <- [0..length css-1] ]
    where
        newStatus :: (Int,Int) -> [String] -> Char
        newStatus (x,y) css | isFloor (querySeat (x,y) css)       = '.'
                            | adjancentSeatsFilled (x,y) css == 0 = '#'
                            | adjancentSeatsFilled (x,y) css >= 4 = 'L'
                            | otherwise = querySeat (x,y) css
adjancentSeatsFilled :: (Int,Int) -> [String] -> Int
adjancentSeatsFilled coord = length . filter isSeatFull . adjancentSeats coord
adjancentSeats :: (Int,Int) -> [String] -> String
adjancentSeats (x,y) css = [ querySeat (x',y') css | x' <- [x-1..x+1], y' <- [y-1..y+1], x' /= x || y' /= y ]
querySeat :: (Int,Int) -> [String] -> Char
querySeat (x,y) css | (x >= 0 && x < length (head css)) && (y >= 0 && y < length css) = css !! y !! x
                    | otherwise = '.'

testData :: [String]
testData = ["L.LL.LL.LL",
            "LLLLLLL.LL",
            "L.L.L..L..",
            "LLLL.LL.LL",
            "L.LL.LL.LL",
            "L.LLLLL.LL",
            "..L.L.....",
            "LLLLLLLLLL",
            "L.LLLLLL.L",
            "L.LLLLL.LL"]

g :: [String] -> Int
g = sum . map (length . filter isSeatFull) . many seatingRound'

seatingRound' :: [String] -> [String]
seatingRound' css = [ [ newStatus (x,y) css | x <- [0..length (head css)-1] ] | y <- [0..length css-1] ]
    where
        newStatus :: (Int,Int) -> [String] -> Char
        newStatus (x,y) css | isFloor (querySeat (x,y) css)     = '.'
                            | visibleSeatsFilled (x,y) css == 0 = '#'
                            | visibleSeatsFilled (x,y) css >= 5 = 'L'
                            | otherwise = querySeat (x,y) css
visibleSeatsFilled :: (Int,Int) -> [String] -> Int
visibleSeatsFilled coord = length . filter isSeatFull . visibleSeats coord
visibleSeats :: (Int,Int) -> [String] -> String
visibleSeats (x,y) css = [ findVisibleSeat (x,y) (i,j) css | i <- [(-1)..1], j <- [(-1)..1], i /= 0 || j /= 0 ]
findVisibleSeat :: (Int,Int) -> (Int,Int) -> [String] -> Char
findVisibleSeat (x,y) (i,j) css | x < 0 || x >= length (head css) || y < 0 || y >= length css = '.'
                                | isFloor (querySeat (x+i,y+j) css) = findVisibleSeat (x+i,y+j) (i,j) css
                                | otherwise = querySeat (x+i,y+j) css

main = do
    contents <- readFile "input/11.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))