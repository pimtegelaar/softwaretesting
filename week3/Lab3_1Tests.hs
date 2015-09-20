module Lab3_1Tests where

import Lab3_1
import Lecture3
import Test.QuickCheck

instance Arbitrary Form where
  arbitrary = elements [form1,form2]
  
-- equivalence is independent of order
testEquiv = quickCheckResult (\ x y -> equiv x y == equiv y x)

-- form cannot be a contradiction and a tautology at the same time
testConTaut = quickCheckResult (\ x -> not (contradiction x && tautology x))