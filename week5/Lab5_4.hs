module Lab5_4

where 

import Data.List
import Lecture5
import Control.Applicative

-- Time spent: 10 hours

-- Findings: it is still possible to generate a Sudoku with 3 empty blocks, not
--           with 4 or more. The minimalize function will fail to clear all values,
--           because it can't guarantee a unique solution anymore.

-- Write a program that generates Sudoku problems with three empty blocks. 
-- Is it also possible to generate Sudoku problems with four empty blocks? Five? 
-- How can you check this?

-- Deliverables: generator, short report on findings, indication of time spent.

blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
sampleNode = (grid2sud (example6), constraints (grid2sud (example6)))

-- example

example6 :: Grid
example6 = [[0,0,0,0,7,0,0,0,0],
            [0,0,0,1,9,5,0,0,0],
            [0,0,0,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

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

genProblem'' :: Node -> IO Node
genProblem'' n = do ns <- genProblem' n
                    xs <- checkProblem' ns
                    if (xs == 3) then 
                      return (ns)
                    else
                      return (emptyN)
                   
checkProblem' :: Node -> IO Int
checkProblem' n = do return (blocksEmpty' n blockConstrnt)
                   
main' :: IO ()
main' = do [r] <- rsolveNs [emptyN]
           showNode r
           s  <- genProblem'' r
           showNode s
          
-- Add checks for verification

-- Same check as below, but for Nodes (usage in IO functions)
blocksEmpty' :: Node -> [[(Row,Column)]] -> Int
blocksEmpty' (s, (c:cs)) [] = 0
blocksEmpty' (s, (_)) (block:blocks) = boolToInt(blockEmpty s block) + (blocksEmpty s blocks)

-- Count the number of empty blocks
-- Example usage: blocksEmpty (grid2sud example4) blockConstrnt
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
