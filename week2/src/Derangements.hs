--
-- Software Testing - Week 2
--
-- Time spent:
--    functions: 1 hour
--    tests: 4 hours
--
module Derangements

where

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
subfac n = (n-1) * (subfac (n-1) + subfac (n-2))
