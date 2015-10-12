module Lab6_4 where

import System.Random
import Lecture6
import Lab6_1
import Lab6_3

-- Test Fermat against list of composites, stop until wrongly identified as Prime
testFermat :: Int -> [Integer] -> Integer
testFermat _ [] = 0
testFermat k (c:cs)
  | prime_tests_F k c == True = c
  | otherwise = testFermat k cs