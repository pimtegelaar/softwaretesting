module Lab5_3

where 

import Data.List
import Lecture5

-- Time spent: 2 hours
-- (probably other test methods are meant, but that is not specified in exercise)

testExample1 = uniqueSol (grid2sud (example1), constraints (grid2sud (example1))) -- True
testExample2 = uniqueSol (grid2sud (example2), constraints (grid2sud (example2))) -- True
testExample3 = uniqueSol (grid2sud (example3), constraints (grid2sud (example3))) -- True
testExample4 = uniqueSol (grid2sud (example4), constraints (grid2sud (example4))) -- False
testExample5 = uniqueSol (grid2sud (example5), constraints (grid2sud (example5))) -- False

-- Should something with 'minimalize' be implemented?
-- We should recursively remove a hint and check if there is still only one solution possible.
-- There are several options of removing hints, we could build up a tree to see which solution leaves the least hints