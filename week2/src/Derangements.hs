--
-- Software Testing - Week 2
--
-- Time spent:
--    functions: 1 hour
--    tests: 4 hours
--
module Derangements

where

import Data.List
import Permutations

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = False
isDerangement xs ys = isPermutation xs ys && isDerangement' xs ys

isDerangement' :: Eq a => [a] -> [a] -> Bool
isDerangement' [] [] = True
isDerangement' (x:xs) (y:ys) = x /= y && isDerangement' xs ys

deran :: Eq a => [a] -> [[a]]
deran xs = [x | x <- perms xs, isDerangement x xs]

-- Returns the number of possible derangements for a list of a given n elements.
subfac :: Int -> Int
subfac 0 = 0
subfac 1 = 0
subfac 2 = 1
subfac n = (n-1) * (subfactorial (n-1) + subfactorial (n-2))

-- Test whether the output of isDerangement matches an expected value.
testIsDerangement :: (Eq a, Show a) => [a] -> [a] -> Bool -> String
testIsDerangement xs ys result = show xs ++ " " ++ show ys ++ " " ++ show result ++ " -> " ++ (if isDerangement xs ys == result then "Passed" else "Failed") ++ "\n"

-- Test isDerangement against all permutations of a given list
-- and returns true if the number of succeeded tests matches the number of possible derangements for that list.
testAgainstPerms :: (Eq a, Show a) => [a] -> String
testAgainstPerms [] = show ""
testAgainstPerms xs = show xs ++ " -> " ++ (if countSucceeded(testAgainstPerms' xs) == subfac (length xs) then "Passed" else "Failed") ++ "\n"

-- Test isDerangement against all permutations of a given list
testAgainstPerms' :: Eq a => [a] -> [Bool]
testAgainstPerms' xs = map (isDerangement xs) (perms xs)  

countSucceeded :: [Bool] -> Int
countSucceeded [] = 0
countSucceeded (x:xs) = (if x == True then 1 else 0) + countSucceeded xs

-- Runs all the tests
tests :: IO ()
tests = putStr (
    testIsDerangement [1] [2] False ++
    testIsDerangement [1] [1] False ++
    testIsDerangement [1,2] [1,2] False ++
    testIsDerangement [1,2] [2,1] True ++
    testIsDerangement [1,2,3] [2,1,3] False ++
    testIsDerangement [1,2,3] [2,3,1] True ++
    testAgainstPerms [1] ++
    testAgainstPerms [1, 2] ++
    testAgainstPerms [1, 2, 3] ++
    testAgainstPerms [1, 2, 3, 4] ++
    testAgainstPerms [1, 2, 3, 4, 5]
    )
