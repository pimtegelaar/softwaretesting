module Lab5_1

where 

import Data.List

type Row    = Int 
type Column = Int
type Value  = Int
type Position = (Row,Column)
type Constrnt = [[Position]]
type Sudoku = (Row,Column) -> Value

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

-- The regular constraints for Sudoku can now be stated as:

rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

-- The generation of the values that are still possible at a given position now takes the following shape:

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let 
   ys = filterConstraint (r,c) xs 
 in 
   foldl1 intersect (map ((values \\) . map s) ys)
       
-- Checks if positions are within the same constraint (block, row, column, etc.)
sameConstraint :: Position -> Position -> Constrnt -> Bool
sameConstraint p1 p2 constraint = filterConstraint p1 constraint == filterConstraint p2 constraint

-- Returns only the partition of the constraint the position is in
filterConstraint :: Position -> Constrnt -> [[Position]]
filterConstraint p constraint = filter (elem p) constraint
