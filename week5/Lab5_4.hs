module Lab5_4

where 

import Data.List
import Control.Monad
import Lecture5

-- Time spent: 4 hours

-- Write a program that generates Sudoku problems with three empty blocks. 
-- Is it also possible to generate Sudoku problems with four empty blocks? Five? 
-- How can you check this?

-- Deliverables: generator, short report on findings, indication of time spent.

sampleNode = (grid2sud (example1), constraints (grid2sud (example1)))
sudoku1 = grid2sud(example1)
sudoku2 = eraseS (sudoku1) (1,1)
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

-- Modify generation of problems with additional block parameters

getRandomBlocks :: [(Row,Column)] -> IO [(Row,Column)]
getRandomBlocks c = randomize c

getRandomBlocks' :: [[(Row,Column)]] -> IO [(Row,Column)]
getRandomBlocks' c = do n <- randomize c
                        return (n !! 0)

-- Generate a random Sudoku

genRandomSudoku' :: IO Node
genRandomSudoku' = do [r] <- rsolveNs [emptyN]
                      return r

randomS' = genRandomSudoku >>= showNode

genProblem' :: Node -> IO Node
genProblem' n = do ys <- getRandomBlocks' $ blockConstrnt
                   return (minimalize n ys)

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem' r
          showNode s
          
-- Add checks for verification