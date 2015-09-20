module Lab3_2 where

import Lecture3
import Testing

-- Time spent: 2 hours
-- No time left to implement something more dynamic, 
-- for example of that, see Lab3_4.hs

testParse :: (String, [Form]) -> Bool
testParse (f, g) = parse f == g

-- If the parsing String does not equal the Form assertion, the test fails

parseTests :: [Test]
parseTests = [ Test "Parse Tests" testParse
             [ ("1+1", [Prop 1])
             , ("(1==>2)", [Impl (Prop 1) (Prop 2)])
             ]
           ]