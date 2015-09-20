module QuickCheckExample where
import Test.QuickCheck
import Lecture3
import Lab3_3

-- Test f commutativity with:
-- quickCheckResult (\ x y -> f x y == f y x)

f :: String -> String -> String
f a b = a ++ " " ++ b

-- Test g commutativity with:
-- quickCheckResult (\ x y -> g x y == g y x)

g :: Int -> Int -> Int
g a b = a + b

data Subject  = Mathematics | Software_Testing | Hans_Subject deriving (Eq,Show,Enum)

instance Arbitrary Subject where
  arbitrary = elements [Mathematics, Software_Testing, Hans_Subject]

-- Test compatible commutativity with:
-- quickCheckResult (\ x y -> compatible x y == compatible y x)

compatible :: Subject -> Subject -> Bool
compatible x y
 | x == y                   = True
 | Mathematics `elem` [x,y] = True
 | otherwise                = False

-- Test badCompatible commutativity with:
-- quickCheckResult (\ x y -> badCompatible x y == badCompatible y x)
 
badCompatible :: Subject -> Subject -> Bool
badCompatible x y
 | x == y                = True
 | x == Software_Testing = True
 | otherwise             = False

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

-- Execute Function:
-- quickCheckResult (\ f -> allVals (cnf (f)) == allVals (f) )
-- verboseCheck (\ f -> allVals (cnf (f)) == allVals (f) )
