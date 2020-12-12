type WestEast = Int
type SouthNorth = Int
type Heading = Int
type Waypoint = (WestEast,SouthNorth)

f :: [String] -> Int
f = (\(we,sn,h) -> abs we + abs sn) . processAllInstr

processAllInstr :: [String] -> (WestEast,SouthNorth,Heading)
processAllInstr = foldr processInstr (0,0,90) . reverse
processInstr :: String -> (WestEast,SouthNorth,Heading) -> (WestEast,SouthNorth,Heading)
processInstr ('N':v) (we,sn,h)   = (we,sn+read v,h)
processInstr ('S':v) (we,sn,h)   = (we,sn-read v,h)
processInstr ('E':v) (we,sn,h)   = (we+read v,sn,h)
processInstr ('W':v) (we,sn,h)   = (we-read v,sn,h)
processInstr ('F':v) (we,sn,0)   = (we,sn+read v,0)
processInstr ('F':v) (we,sn,90)  = (we+read v,sn,90)
processInstr ('F':v) (we,sn,180) = (we,sn-read v,180)
processInstr ('F':v) (we,sn,270) = (we-read v,sn,270)
processInstr ('R':v) (we,sn,h)   = (we,sn,(h+read v) `mod` 360)
processInstr ('L':v) (we,sn,h)   = (we,sn,(h-read v) `mod` 360)

testData :: [String]
testData = ["F10","N3","F7","R90","F11"]

g :: [String] -> Int
g = (\(we,sn,w) -> abs we + abs sn) . processAllInstrWay

processAllInstrWay :: [String] -> (WestEast,SouthNorth,Waypoint)
processAllInstrWay = foldr processInstrWay (0,0,(10,1)) . reverse
processInstrWay :: String -> (WestEast,SouthNorth,Waypoint) -> (WestEast,SouthNorth,Waypoint)
processInstrWay ('N':v) (we,sn,(wwe,wsn)) = (we,sn,(wwe,wsn+read v))
processInstrWay ('S':v) (we,sn,(wwe,wsn)) = (we,sn,(wwe,wsn-read v))
processInstrWay ('E':v) (we,sn,(wwe,wsn)) = (we,sn,(wwe+read v,wsn))
processInstrWay ('W':v) (we,sn,(wwe,wsn)) = (we,sn,(wwe-read v,wsn))
processInstrWay ('F':v) (we,sn,(wwe,wsn)) = (we+wwe*read v,sn+wsn*read v,(wwe,wsn))
processInstrWay (d:v)   (we,sn,(wwe,wsn)) = (we,sn,rotateWaypoint (d,read v) (wwe,wsn))
    where
        rotateWaypoint :: (Char,Int) -> Waypoint -> Waypoint
        rotateWaypoint rot (wwe,wsn) | angleDiff rot == 0   = (wwe,wsn)
        rotateWaypoint rot (wwe,wsn) | angleDiff rot == 90  = (wsn,-wwe)
        rotateWaypoint rot (wwe,wsn) | angleDiff rot == 180 = (-wwe,-wsn)
        rotateWaypoint rot (wwe,wsn) | angleDiff rot == 270 = (-wsn,wwe)
        angleDiff :: (Char,Int) -> Int
        angleDiff ('R',v) = v `mod` 360
        angleDiff ('L',v) = (-v) `mod` 360

main = do
    contents <- readFile "input/12.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))