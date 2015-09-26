module Lab4_2 where

import SetOrd
import System.Random
import Test.QuickCheck
import Lecture2

-- Time Spent: 2 hours

-- Assignment: 
-- Implement a random data generator for the datatype Set Int, where Set is as 
-- defined in SetOrd.hs. First do this from scratch, next give a version that uses 
-- QuickCheck to random test this datatype.
-- (Deliverables: two random test generators, indication of time spent.)

-- Create Set of Int
  
setInt :: [Int] -> Set Int
setInt i = Set i

-- Random Generator

isSameSet :: Set Int -> Set Int -> Bool
isSameSet a b = a == b

-- Union results in deviating Set?

isIdempotent :: Set Int -> Set Int -> Bool
isIdempotent a b = unionSet a b == a

testSet :: Int -> Int -> ([Int] -> Set Int)
                      -> (Set Int -> Set Int -> Bool) -> IO ()
testSet k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  if r (Set xs) (f xs) then
                    do print ("pass on: " ++ show xs)
                       testSet (k+1) n f r
                  else error ("failed test on: " ++ show xs)

testSets :: ([Int] -> Set Int) -> (Set Int -> Set Int -> Bool) -> IO ()
testSets f p = testSet 1 10 f p

-- QuickCheck