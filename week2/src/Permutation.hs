{-# LANGUAGE OverloadedStrings #-}
module Permutation where

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = length x == length y && containsAll x y && containsAll y x

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll [] _ = True
containsAll _ [] = False
containsAll (x:xs) y = x `elem` y && containsAll xs y

test ::  (Eq a, Show a) => [a] -> [a] -> Bool -> String
test x y result = show x ++ " " ++ show y ++ " == " ++ show result ++ " -> " ++ (if isPermutation x y == result then "Passed" else "Failed") ++ "\n"

tests :: IO ()
tests = putStr (
    test [] [3, 2, 1] False ++ -- x empty
    test [3, 2, 1] [] False ++ -- y empty
    test [1, 2] [3, 2, 1] False ++ -- x smaller 
    test [1, 2, 3] [3, 2] False ++ -- y smaller
    test [1, 2, 3] [3, 2, 3] False ++ -- y missing value from x
    test [3, 2, 3] [3, 2, 1] False ++ -- x missing value from y
    test [1, 2, 3] [3, 2, 4] False ++ -- y has number not in x and x has number not in y
    test [1, 2, 3] [3, 2, 1] True ++ -- reversed
    test [1, 2, 3] [1, 2, 3] True ++ -- equal
    test [1, 2, 3] [3, 1, 2] True  -- different order
    )
    
perms :: (Eq a, Show a) => [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (insrt x) (perms xs) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)
  
testPerms :: (Eq a, Show a) => [a] -> [[a]] -> String
testPerms [] _ = ""
testPerms (_ : _) [] = ""
testPerms x (y:ys) = test x y True ++ testPerms x ys

testAllPerms :: (Show a, Eq a) => [a] -> IO ()
testAllPerms x = putStr(testPerms x (perms x)) -- confirms that all permutations of a list are permutations...

