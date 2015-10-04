module Lab5_2

where 

import Data.List
import Lecture5

-- Time spent: 12 hours
-- (unclear yet how the type modifications should be reflected by the functions, no proper implementation yet)

type Position = (Row,Column)
type Constrnt = [[Position]]

-- The regular constraints for Sudoku can now be stated as:

rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

allConstraints = [rowConstrnt,columnConstrnt,blockConstrnt]

-- The generation of the values that are still possible at a given position now takes the following shape:

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let 
   ys = filterConstraint (r,c) xs 
 in 
   foldl1 intersect (map ((values \\) . map s) ys)
   
freeAtPosition :: Sudoku -> Position -> [Value]
freeAtPosition s p = freeAtPosition' s p allConstraints   
   
freeAtPosition' :: Sudoku -> Position -> [Constrnt] -> [Value]
freeAtPosition' s p [] = freeAtPos' s p rowConstrnt
freeAtPosition' s p (c:cs) = (freeAtPos' s p c) `intersect` (freeAtPosition' s p cs)
   
       
-- Checks if positions are within the same constraint (block, row, column, etc.)
sameConstraint :: Position -> Position -> Constrnt -> Bool
sameConstraint p1 p2 constraint = filterConstraint p1 constraint == filterConstraint p2 constraint

-- Returns only the partition of the constraint the position is in
filterConstraint :: Position -> Constrnt -> [[Position]]
filterConstraint p constraint = filter (elem p) constraint

-- Alternative version
sameblock2 :: Position -> Position -> Constrnt -> Bool
sameblock2 _ _ [] = False
sameblock2 p1 p2 (c:xs) = ((p1 `elem` c) && (p2 `elem` c)) || sameblock2 p1 p2 xs
