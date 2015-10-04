module Lab5_3

where 

import Data.List
import Lecture5

-- Time spent: 2 hours
-- (probably other test methods are meant, but that is not specified in exercise)

testExample1 = uniqueSol (grid2sud (example1), constraints (grid2sud (example1)))
testExample2 = uniqueSol (grid2sud (example2), constraints (grid2sud (example2)))
testExample3 = uniqueSol (grid2sud (example3), constraints (grid2sud (example3)))
testExample4 = uniqueSol (grid2sud (example4), constraints (grid2sud (example4)))
testExample5 = uniqueSol (grid2sud (example5), constraints (grid2sud (example5)))