--
-- Software Testing - Week 2
-- Erik Verhoofstad
--
-- Time spent:
--   functions: 1 hour
--
module Permutations

where

import Data.List

-- Determine whether 2 lists are permutations of each other
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys = in_list x ys && isPermutation xs (delete x ys)

-- Determine whether an given argument is an element of a given list.
in_list :: Eq a => a -> [a] -> Bool
in_list _ [] = False
in_list a (x:xs) = a == x || in_list a xs

-- Give all the permutations from a given list. (from Workshop 2)
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)
