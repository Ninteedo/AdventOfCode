type SeatRange = ((Int,Int),(Int,Int)) -- ((lowerRowLimit, upperRowLimit), (lowerColLimit, upperColLimit))
type Seat = (Int,Int) -- (row, col)

f :: [String] -> Int
f = maximum . map (seatID . findSeat)
    where
        seatID :: Seat -> Int
        seatID (row,col) = row * 8 + col

findSeat :: String -> Seat
findSeat = finishSeat . foldr updateSeat ((1,rowUpper),(1,colUpper)) . reverse
    where
        rowUpper, colUpper :: Int
        rowUpper = 128
        colUpper = 8
        updateSeat :: Char -> SeatRange -> SeatRange
        updateSeat 'F' ((lR,uR),(lC,uC)) = ((lR,uR-(uR-lR+1)`div`2),(lC,uC))
        updateSeat 'B' ((lR,uR),(lC,uC)) = ((lR+(uR-lR+1)`div`2,uR),(lC,uC))
        updateSeat 'L' ((lR,uR),(lC,uC)) = ((lR,uR),(lC,uC-(uC-lC+1)`div`2))
        updateSeat 'R' ((lR,uR),(lC,uC)) = ((lR,uR),(lC+(uC-lC+1)`div`2,uC))
        finishSeat :: SeatRange -> Seat
        finishSeat ((lR,uR),(lC,uC)) | lR == uR && lC == uC = (lR - 1, lC - 1)
                                     | otherwise = error "SeatRange not specific so can't find Seat"

g :: [String] -> [Seat]
g = pruneNotReal . findMissing . map findSeat
    where
        findMissing :: [Seat] -> [Seat]
        findMissing seats = [ (r,c) | r <- [0..127], c <- [0..7], (r,c) `notElem` seats ]
        pruneNotReal :: [Seat] -> [Seat]
        pruneNotReal = reverse . dropInvalid . reverse . dropInvalid
        dropInvalid :: [Seat] -> [Seat]
        dropInvalid seats = map fst (dropWhile invalid (zip seats $ tail seats))
            where
                invalid :: (Seat,Seat) -> Bool
                invalid ((row1,col1),(row2,col2)) = (row1 <= row2 + 1 && row1 + 1 >= row2) || (row2 <= row1 + 1 && row2 + 1 >= row1)

main = do
    contents <- readFile "input/05.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))