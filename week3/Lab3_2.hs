module Lab3_2 where

import Lecture3
import Testing

-- Time spent:  2 hours
-- Test method: The test method used is using assertions, the String formula is compared 
--              with the parse output Form.
-- Test output: The output of the tests is an empty list, which means it succeeded.
--              However, no counter examples are generated this way. This way the
--              correct examples are tested, but the incorrect are ignored. Also
--              the set of tests is limited. Each time the same Strings are tested.
--              For some purposes this is good (random tests are not always a good
--              thing), but in this case it restricts the amount of testcases.

testParse :: (String, [Form]) -> Bool
testParse (f, g) = parse f == g

-- If the parsing String does not equal the Form assertion, the test fails
parseTests :: [Test]
parseTests = [ Test "Parse Tests" testParse
             [ ("1+1", [Prop 1])
             , ("(1==>2)", [Impl (Prop 1) (Prop 2)])
             , ("(3<=>4)", [Equiv (Prop 3) (Prop 4)])
                         , ("-5", [Neg (Prop 5)])
             ]
           ]

-- Yes, bad naming convention, sorry
runEverythingInTheWorld = runTests parseTests