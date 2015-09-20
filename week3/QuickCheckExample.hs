module QuickCheckExample where
import Test.QuickCheck

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
