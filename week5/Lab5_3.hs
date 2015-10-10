module Lab5_3

where 

import Data.List
import Lecture5

-- Time spent: 4 hours

uniqueSolSud s = uniqueSol (s, constraints s)
uniqueSolGrid g = uniqueSolSud (grid2sud (g))

testUniqueSolExample1 = uniqueSolGrid example1 -- True
testUniqueSolExample2 = uniqueSolGrid example2 -- True
testUniqueSolExample3 = uniqueSolGrid example3 -- True
testUniqueSolExample4 = uniqueSolGrid example4 -- False
testUniqueSolExample5 = uniqueSolGrid example5 -- False

grid2Node :: Grid -> Node
grid2Node g = sudoku2Node (grid2sud g)

sudoku2Node :: Sudoku -> Node
sudoku2Node s = (s, (constraints s))
  
mm :: Node -> Node
mm n = minimalize n (filledPositions (fst n))

sudEquals :: Sudoku -> Sudoku -> Bool
sudEquals s1 s2 = sud2grid s1 == sud2grid s2

isMinimized :: Sudoku -> Bool
isMinimized s = uniqueSolSud s && sudEquals s (fst (mm (sudoku2Node s)))

testMinimizedExample1 = isMinimized (grid2sud example1) -- False
testMinimizedExample2 = isMinimized (grid2sud example2) -- False
testMinimizedExample3 = isMinimized (grid2sud example3) -- True
testMinimizedExample4 = isMinimized (grid2sud example4) -- False
testMinimizedExample5 = isMinimized (grid2sud example5) -- False

{--
 Minimalize is already included in genProblem. 
 In order to test if genProblem results in a minimalized solution, you could run minimalize on it again,
 to see if the result stays the same.
 However, genProblem results in an IO Node, which cannot be used again as input for the minimalize function. 
--} 
