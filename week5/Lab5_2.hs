module Lab5_2

where 

import Data.List
import System.Random

-- Time spent: 16 hours

-- Definitions

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]
type Position = (Row,Column)
type Constrnt = [[Position]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

nrcBlocks :: [[Int]]
nrcBlocks = [[2..4],[6..8]]

rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
nrcConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- nrcBlocks, b2 <- nrcBlocks ]

allConstraints = [rowConstrnt,columnConstrnt,blockConstrnt]


-- Showing stuff
showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")

-- Adding Sudoku
    
type Sudoku = (Row,Column) -> Value

-- Conversion Functions

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

-- Show Sudoku
  
showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid


-- Check free Values

freeAtPos :: Sudoku -> Position -> [Value]
freeAtPos s p = freeAtPos' s p allConstraints
   
freeAtPos' :: Sudoku -> Position -> [Constrnt] -> [Value]
freeAtPos' _ _ [] = [ x | x <- values]
freeAtPos' s p (c:cs) = (freeAtPos2 s p c) `intersect` (freeAtPos' s p cs)
  
freeAtPos2 :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos2 s (r,c) xs = let 
   ys = filter (elem (r,c)) xs 
 in 
   foldl1 intersect (map ((values \\) . map s) ys)
   

-- Returns a list of filled values of a given list of positions   
getValues :: Sudoku -> [Position] -> [Value]
getValues s ps = filter (/=0) (map s ps)


consistent :: Sudoku -> Bool
consistent s = all (\c -> consistent' s c) allConstraints

consistent' :: Sudoku -> Constrnt -> Bool
consistent' _ [] = True
consistent' s (p:ps) = getValues s p == (nub (getValues s p)) && (consistent' s ps)


consistentPos :: Sudoku -> Position -> Bool
consistentPos s p = all (\c -> consistent' s c) (constraintsForPos p)

constraintsForPos :: Position -> [Constrnt]
constraintsForPos p = [ constraintsForPos' p c | c <- allConstraints ]

constraintsForPos' :: Position -> Constrnt -> Constrnt
constraintsForPos' p c = [ sc | sc <- c, p `elem` sc ]

          
-- Extension
                    
extend :: Sudoku -> (Position, Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 

-- Searching for Solution

-- A node represents a (partially) filled Sudoku and the remaining open positions.
type Node = (Sudoku, [Position])


showNode :: Node -> IO()
showNode = showSudoku . fst

-- If there are no more open positions, the Sudoku is solved.
solved  :: Node -> Bool
solved = (==[]) . snd

-- The successors of a node are the nodes where the first next open position 
-- is filled with all possible values that don't break any constraint.
extendNode :: Node -> [Node]
extendNode (s, (p:positions)) = [ (extend s (p,v), positions) | v <- values, consistentPos (extend s (p,v)) p ]

--extendNode :: Node -> [Node]
--extendNode (s, positions) = 
--   [(extend s ((r,c),v),
--     sortBy length3rd $ 
--         prune (r,c,v) positions) | v <- vs ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

-- prune :: (Row,Column,Value) 
      -- -> [Constraint] -> [Constraint]
-- prune _ [] = []
-- prune (r,c,v) ((x,y,zs):rest)
  -- | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  -- | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  -- | sameblock (r,c) (x,y) = 
        -- (x,y,zs\\[v]) : prune (r,c,v) rest
  -- | otherwise = (x,y,zs) : prune (r,c,v) rest

       
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



initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, openPositions s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

                       
-- Generic depth first search algorithm

search :: (node -> [node]) -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs) 
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

-- Searching with Nodes
  
solveNs :: [Node] -> [Node]
solveNs = search succNode solved 

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,ps) = extendNode (s,ps) 

-- Solve Functions

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs = sequence . fmap showNode . solveNs

-- Generate Sudokus
            
-- emptyN :: Node
-- emptyN = (\ _ -> 0,constraints (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y 
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

-- sameLen :: Constraint -> Constraint -> Bool
-- sameLen (_,_,xs) (_,_,ys) = length xs == length ys

-- getRandomCnstr :: [Constraint] -> IO [Constraint]
-- getRandomCnstr cs = getRandomItem (f cs) 
  -- where f [] = []
        -- f (x:xs) = takeWhile (sameLen x) (x:xs)

-- rsuccNode :: Node -> IO [Node]
-- rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      -- if null xs 
                        -- then return []
                        -- else return 
                          -- (extendNode (s,cs\\xs) (head xs))

-- rsolveNs :: [Node] -> IO [Node]
-- rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node]) 
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs 
       then return []
       else 
         if goal (head xs) 
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys 
                      then return [head ys]
                      else if null (tail xs) then return []
                           else 
                             rsearch 
                               succ goal (return $ tail xs)


-- genRandomSudoku :: IO Node
-- genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     -- return r

-- randomS = genRandomSudoku >>= showNode

-- uniqueSol :: Node -> Bool
-- uniqueSol node = singleton (solveNs [node]) where 
  -- singleton [] = False
  -- singleton [x] = True
  -- singleton (x:y:zs) = False

eraseS :: Sudoku -> Position -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

-- eraseN :: Node -> (Row,Column) -> Node
-- eraseN n (r,c) = (s, constraints s) 
  -- where s = eraseS (fst n) (r,c) 

-- minimalize :: Node -> [(Row,Column)] -> Node
-- minimalize n [] = n
-- minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                         -- | otherwise    = minimalize n  rcs
  -- where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]




-- Sudoku Examples

example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8], 
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

exampleNrc :: Grid
exampleNrc = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]]

              
-- genProblem :: Node -> IO Node
-- genProblem n = do ys <- randomize xs
                  -- return (minimalize n ys)
   -- where xs = filledPositions (fst n)

-- main :: IO ()
-- main = do [r] <- rsolveNs [emptyN]
          -- showNode r
          -- s  <- genProblem r
          -- showNode s