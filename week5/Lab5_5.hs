module Lab5_5

where 

import Data.List
import Lab5_1

-- Time spent: 1 hours

-- Generate NRC Sudoku
genNrc :: IO ()
genNrc = do s <- genRandomSudoku
            (genProblem s) >>= showNode
