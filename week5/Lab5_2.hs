module Lab5_2

where 

import Data.List
import Lecture5

-- Time spent: 12 hours
-- (unclear yet how the type modifications should be reflected by the functions, no proper implementation yet)

type Position = (Row,Column)
type Constrnt = [[Position]]

nrcBlocks :: [[Int]]
nrcBlocks = [[2..4],[6..8]]

-- The regular constraints for Sudoku can now be stated as:

rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
nrcConstrnt = [[(r,c) | r <- b1, c <- b2 ] | b1 <- nrcBlocks, b2 <- nrcBlocks ]

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
freeAtPosition' _ _ [] = [ x | x <- values]
freeAtPosition' s p (c:cs) = (freeAtPos' s p c) `intersect` (freeAtPosition' s p cs)


getValues :: Sudoku -> [Position] -> [Value]
getValues s ps = filter (/=0) (map s ps)


consistent1 :: Sudoku -> Bool
consistent1 s = consistent' s allConstraints

consistent' :: Sudoku -> [Constrnt] -> Bool
consistent' _ [] = True
consistent' s (c:cs) = (consistent2 s c) && (consistent' s cs) 


consistent2 :: Sudoku -> Constrnt -> Bool
consistent2 _ [] = True
consistent2 s (p:ps) = getValues s p == (nub (getValues s p)) && (consistent2 s ps)


   
       
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
