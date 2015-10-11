module Lab5_4

where 

import Data.List
import Lecture5

-- Time spent: 12 hours

blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
sampleNode = (grid2sud (example6), constraints (grid2sud (example6)))

-- example with 1 empty block
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
            
emptyBlocks :: Int
emptyBlocks = 3

-- Generate 3 random blocks
getRandomBlocks :: [[(Row,Column)]] -> IO [(Row,Column)]
getRandomBlocks c = do n <- randomize c
                       return (concat (take emptyBlocks n))

-- Generate (random) problems until it matches 3 empty blocks
genProblem' :: Node -> IO Node
genProblem' n = do ns <- genRandomProblem n
                   c <- checkProblem ns
                   if (c == emptyBlocks) then 
                     return (ns)
                   else
                     do [r] <- rsolveNs [emptyN]
                        showNode r
                        s  <- genProblem' r
                        return s

-- Generate a problem with 3 empty blocks
genRandomProblem :: Node -> IO Node
genRandomProblem n = do ys <- getRandomBlocks $ blockConstrnt
                        return (minimalize n ys)
                 
checkProblem :: Node -> IO Int
checkProblem n = do return (blocksEmptyN n blockConstrnt)
                   
main' :: IO ()
main' = do [r] <- rsolveNs [emptyN]
           showNode r
           s  <- genProblem' r
           showNode s
          
-- Add checks for verification

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

-- Same check as blocksEmpty, but for Nodes (usage in IO functions)
blocksEmptyN :: Node -> [[(Row,Column)]] -> Int
blocksEmptyN (s, (c:cs)) [] = 0
blocksEmptyN (s, (_)) (block:blocks) = boolToInt(blockEmpty s block) + (blocksEmpty s blocks)

{--
 Findings:
 
 A good amount of empty blocks is 3. It is still possible to generate
 a Sudoku with 4 empty blocks, but this is much more difficult to find.
 The minimalize function will fail in the 4 block case for most
 generated problems, because it can't always guarantee a unique solution.
 It may run for quite a while. For three empty blocks it is done in a few
 tries. We check it now by just generating new problems. If it keeps
 running we assume that it is not possible with this amount of empty blocks.
--} 