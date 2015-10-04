module Lab5_5

where 

import Data.List
import Lab5_1

-- Time spent: ? hours

-- This works but very, very slowly.
genNrc :: IO ()
genNrc = do s <- genRandomSudoku
            (genProblem s) >>= showNode
