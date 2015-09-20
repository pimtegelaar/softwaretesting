module Lab3_1Tests where

import Lab3_1
import Lecture3
import Test.QuickCheck

instance Arbitrary Form where
  arbitrary = elements [form1,form2]
  
-- equivalence is independent of order
testEquivInAnyOrder = quickCheckResult (\ x y -> equiv x y == equiv y x)

-- form cannot be a contradiction and a tautology at the same time
testNotConAndTaut = quickCheckResult (\ x -> not (contradiction x && tautology x))

-- form cannot be a contradiction and satisfiable at the same time
testNotConAndSat = quickCheckResult (\ x -> not (contradiction x && satisfiable x))