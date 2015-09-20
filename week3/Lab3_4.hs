module Lab3_4 where

import Test.QuickCheck
import Lecture3
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

checkQuick :: (Form -> [Valuation]) -> (Form -> Form) -> IO Result
checkQuick g h = quickCheckResult (\ f -> g (h (f)) == g (f) )

-- Execute following Function to compare truth tables against arbitrary formulas:
-- quickCheckResult (\ f -> allVals (cnf (f)) == allVals (f) )

-- With intermediate results:
-- verboseCheck (\ f -> allVals (cnf (f)) == allVals (f) )
