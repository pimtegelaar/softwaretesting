module Lab5_4

where 

import Data.List
import Control.Applicative
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

minimalize' :: Node -> [(Row,Column)] -> Node
minimalize' n [] = n
minimalize' n ((r,c):rcs) | uniqueSol n' = minimalize' n' rcs
                          | otherwise    = minimalize' n  rcs
  where n' = eraseN n (r,c)
  
getBlock :: Int -> [(Row,Column)]
getBlock n = blockConstrnt !! (n - 1)

-- Generate a random Sudoku

-- getRandomInt :: Int -> IO Int
-- getRandomInt n = getStdRandom (randomR (0,n))

-- getRandomItem :: [a] -> IO [a]
-- getRandomItem [] = return []
-- getRandomItem xs = do n <- getRandomInt maxi
                      -- return [xs !! n]
                   -- where maxi = length xs - 1

-- randomize :: Eq a => [a] -> IO [a]
-- randomize xs = do y <- getRandomItem xs 
                  -- if null y 
                    -- then return []
                    -- else do ys <- randomize (xs\\y)
                            -- return (head y:ys)

genRandomSudoku' :: IO Node
genRandomSudoku' = do [r] <- rsolveNs [emptyN]
                      return r

randomS' = genRandomSudoku >>= showNode

genProblem' :: Node -> IO Node
-- genProblem' n = do ys <- randomize xs
genProblem' n = do ys <- randomize . getBlock $ 6
                   return (minimalize' n ys)
    -- where xs = filledPositions (fst n) 

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem' r
          showNode s
          
-- Add checks for verification