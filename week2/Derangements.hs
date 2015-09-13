--
-- Software Testing - Week 2
-- Erik Verhoofstad
--
-- Time spent:
--    functions: 1 hour
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
