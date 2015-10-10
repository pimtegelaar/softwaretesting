module Lab5_4

where 

import Data.List
import Lecture5

-- Time spent: 7 hours

-- Write a program that generates Sudoku problems with three empty blocks. 
-- Is it also possible to generate Sudoku problems with four empty blocks? Five? 
-- How can you check this?

-- Deliverables: generator, short report on findings, indication of time spent.

blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

-- Modify generation of problems with additional block parameters

getRandomBlocks :: [[(Row,Column)]] -> IO [(Row,Column)]
getRandomBlocks c = do n <- randomize c
                       return (concat (take 3 n))

-- Generate a random Sudoku

genRandomSudoku' :: IO Node
genRandomSudoku' = do [r] <- rsolveNs [emptyN]
                      return r

randomS' = genRandomSudoku >>= showNode

genProblem' :: Node -> IO Node
genProblem' n = do ys <- getRandomBlocks $ blockConstrnt
                   return (minimalize n ys)

main' :: IO ()
main' = do [r] <- rsolveNs [emptyN]
           showNode r
           s  <- genProblem' r
           showNode s
          
-- Add checks for verification

-- Count the number of empty blocks
blocksEmpty :: Sudoku -> [[(Row,Column)]] -> Int
blocksEmpty s [] = 0
blocksEmpty s (block:blocks) = boolToInt(blockEmpty s block) + (blocksEmpty s blocks)

--Check if a block is empty
blockEmpty :: Sudoku -> [(Row,Column)] -> Bool
blockEmpty s [] = True
blockEmpty s (p:ps) = s p == 0 && blockEmpty s ps

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True  = 1
