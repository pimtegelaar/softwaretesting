module Lab3_4 where

import Test.QuickCheck
import Lecture3
import Lab3_1
import Lab3_3

-- Try with set of fixed formulas to compare truth tables between formulas 
-- with CNF formulas.

a = Prop 1
b = Prop 2
c = Prop 3 

form4 = Impl a b
form5 = Cnj [a, b]
form6 = Dsj [a, b]
form7 = Equiv a b
form8 = Neg a
form9 = Prop 1
form10 = Impl form8 form7

instance Arbitrary Form where
  arbitrary = elements [form4, form5, form6, form7, form8, form9, form10]

-- Compare truth tables for CNF formulas with arbitrary formulas
quickCheckTruthTable = quickCheckResult (\ f -> allVals (cnf (f)) == allVals (f) )

-- With intermediate results
quickCheckTruthTableVerbose = verboseCheck (\ f -> allVals (cnf (f)) == allVals (f) )

-- Compare CNF and arbitrary formulas with regard to satisfiability
quickCheckSatisfiable = quickCheckResult (\ f -> satisfiable (cnf (f)) == satisfiable (f) )

-- Compare CNF and arbitrary formulas with regard to contradictions
quickCheckContradiction = quickCheckResult (\ f -> contradiction (cnf (f)) == contradiction (f) )

-- Attempt to make dynamic check
checkQuick :: Eq a => (Form -> a) -> (Form -> Form) -> IO Result
checkQuick g h = quickCheckResult (\ f -> g (h (f)) == g (f) )

-- Verbose version
checkVerbose :: Eq a => (Form -> a) -> (Form -> Form) -> IO ()
checkVerbose g h = verboseCheck (\ f -> g (h (f)) == g (f) )