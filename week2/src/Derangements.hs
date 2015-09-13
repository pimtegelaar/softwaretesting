--
-- Software Testing - Week 2
--
-- Time spent:
--    functions: 1 hour
--    tests: 3 hours
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

-- Test isDerangement against all permutations of a given list
-- and returns true if the number of succeeded tests matches the number of possible derangements for that list.
testIsDerangement :: Eq a => [a] -> Bool
testIsDerangement [] = False
testIsDerangement xs = countSucceeded(testIsDerangement' xs) == subfac (length xs)

-- Test isDerangement against all permutations of a given list
testIsDerangement' :: Eq a => [a] -> [Bool]
testIsDerangement' xs = map (isDerangement xs) (perms xs)  

countSucceeded :: [Bool] -> Int
countSucceeded [] = 0
countSucceeded (x:xs) = (if x == True then 1 else 0) + countSucceeded xs

test :: (Eq a, Show a) => [a] -> String
test xs = show xs ++ " -> " ++ (if testIsDerangement xs then "Passed" else "Failed") ++ "\n"

tests :: IO ()
tests = putStr (
    test [1] ++
    test [1, 2] ++
    test [1, 2, 3] ++
    test [1, 2, 3, 4] ++
    test [1, 2, 3, 4, 5]
    )
