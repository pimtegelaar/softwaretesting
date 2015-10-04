module Lab5_4

where 

import Data.List
import Control.Monad
import Lecture5

-- Time spent: 3 hours

-- Write a program that generates Sudoku problems with three empty blocks. 
-- Is it also possible to generate Sudoku problems with four empty blocks? Five? 
-- How can you check this?

-- Deliverables: generator, short report on findings, indication of time spent.

sampleNode = (grid2sud (example1), constraints (grid2sud (example1)))
sudoku1 = grid2sud(example1)
sudoku2 = eraseS (sudoku1) (1,1)

-- Generate a random Sudoku

genRandomSudoku' :: IO Node
genRandomSudoku' = do [r] <- rsolveNs [emptyN]
                      return r

randomS' = genRandomSudoku >>= showNode

-- Modify generation of problems with additional block parameters

minimalize' :: Node -> [(Row,Column)] -> Node
minimalize' n [] = n
minimalize' n ((r,c):rcs) | uniqueSol n' = minimalize' n' rcs
                          | otherwise    = minimalize' n  rcs
  where n' = eraseN n (r,c)

genProblem' :: Node -> IO Node
genProblem' n = do ys <- randomize xs
                   return (minimalize' n ys)
    where xs = filledPositions (fst n)
    
--randomP' = fmap (genProblem') (rsolveNs)

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
--          s  <- genProblem' r
--          showNode s

-- Attempt to erase entire block
eraseB :: Sudoku -> [(Row,Column)] -> Sudoku
eraseB s [(r,c)] (x,y) | (r,c) == (x,y) = 0
                       | otherwise      = s (x,y)
                     
-- Add checks for verification