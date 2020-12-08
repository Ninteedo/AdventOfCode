data Instruction = Acc | Jmp | Nop
type GlobalValue = Int
type ModValue = Int
type LineNumber = Int

f :: [String] -> Int
f = executeProgramGVal (0,0,[]) . map toProgramLine

executeProgramTrace :: (LineNumber,GlobalValue,[LineNumber]) -> [(Instruction,ModValue)] -> [LineNumber]
executeProgramTrace (lNum,gVal,lNums) ls | lNum `elem` lNums = lNums ++ [lNum]
                                         | lNum >= length ls = lNums ++ [-1]
                                         | otherwise = executeProgramTrace (executeLine (ls !! lNum, lNum, gVal, lNums)) ls
executeProgramGVal :: (LineNumber,GlobalValue,[LineNumber]) -> [(Instruction,ModValue)] -> Int
executeProgramGVal  (lNum,gVal,lNums) ls | lNum `elem` lNums || lNum >= length ls = gVal
                                         | otherwise = executeProgramGVal (executeLine (ls !! lNum, lNum, gVal, lNums)) ls

executeLine :: ((Instruction,ModValue),LineNumber,GlobalValue,[LineNumber]) -> (LineNumber,GlobalValue,[LineNumber])
executeLine ((Acc,mVal),lNum,gVal,lNums) = (lNum+1,gVal+mVal,lNums++[lNum])
executeLine ((Jmp,mVal),lNum,gVal,lNums) = (lNum+mVal,gVal,lNums++[lNum])
executeLine ((Nop,mVal),lNum,gVal,lNums) = (lNum+1,gVal,lNums++[lNum])
toProgramLine :: String -> (Instruction,ModValue)
toProgramLine xs | sign == '+' = (strToInstr instr,read val :: Int)
                 | sign == '-' = (strToInstr instr,(-1) * read val :: Int)
    where
        (instr,' ':sign:val) = break (==' ') xs
        strToInstr :: String -> Instruction
        strToInstr "acc" = Acc
        strToInstr "jmp" = Jmp
        strToInstr "nop" = Nop

g :: [String] -> Int
g = executeProgramGVal (0,0,[]) . head . filter successful . alternatePrograms . map toProgramLine
    where
        alternatePrograms :: [(Instruction,ModValue)] -> [[(Instruction,ModValue)]]
        alternatePrograms ls = [ [ limitChangeLine (i==j) (ls!!j) | j <- [0..length ls - 1] ] | i <- [0..length ls - 1] ]
        limitChangeLine :: Bool -> (Instruction,ModValue) -> (Instruction,ModValue)
        limitChangeLine True l = changeLine l
        limitChangeLine False l = l
        successful :: [(Instruction,ModValue)] -> Bool
        successful ls = last (executeProgramTrace (0,0,[]) ls) == (-1)
        changeLine :: (Instruction,ModValue) -> (Instruction,ModValue)
        changeLine (Acc,mVal) = (Acc,mVal)
        changeLine (Jmp,mVal) = (Nop,mVal)
        changeLine (Nop,mVal) = (Jmp,mVal)

main = do
    contents <- readFile "input/8.txt"
    let ss = lines contents
    print ("#1: " ++ show (f ss))
    print ("#2: " ++ show (g ss))