module Lab5_4

where 

import Data.List
import Lecture5

-- Time spent: ? hours

-- Write a program that generates Sudoku problems with three empty blocks. 
-- Is it also possible to generate Sudoku problems with four empty blocks? Five? 
-- How can you check this?

-- Deliverables: generator, short report on findings, indication of time spent.

sampleNode = (grid2sud (example1), constraints (grid2sud (example1)))

-- Modify generation of problems with additional block parameters

genProblem' :: Node -> IO Node
genProblem' n = do ys <- randomize xs
                   return (minimalize n ys)
    where xs = filledPositions (fst n)

-- Add implementation of Main method

--main' :: IO ()
--main' = do [r] <- rsolveNs [emptyN]
--           showNode r
--           s  <- genProble' r
--           showNode s

-- Add check for verification