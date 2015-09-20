module Lab3_4 where

import Test.QuickCheck
import Lecture3
import Control.Applicative
import Lab3_1
import Lab3_3

-- Generate random set of formulas which can be used in QuickCheck
instance Arbitrary Form where
  arbitrary = intToForm2 <$> choose (1,5) <*> choose (1,100) <*> choose (1,100)

-- Create formulas from Ints
intToForm1 :: Int -> Form
intToForm1 a = Neg (Prop a)

intToForm2 :: Int -> Int -> Int -> Form
intToForm2 1 a b = Impl (Prop a) (Prop b)
intToForm2 2 a b = Equiv (Prop a) (Prop b)
intToForm2 3 a b = Dsj [(Prop a), (Prop b)]
intToForm2 4 a b = Cnj [(Prop a), (Prop b)]
intToForm2 5 a b = Impl (intToForm1 a) (intToForm1 b)  

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
