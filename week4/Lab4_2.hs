module Lab4_2 where

import SetOrd
import System.Random
import Test.QuickCheck

-- Time Spent: 5 hours

-- Helper functions

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
  b <- getRandomInt 1
  if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
  x <-  getRandomInt k
  y <- randomFlip x
  xs <- getIntL k (n-1)
  return (y:xs)

-- Create Set of Int
  
setInt :: [Int] -> Set Int
setInt i = Set i

-- Random Generator

isSameSet :: Set Int -> Set Int -> Bool
isSameSet a b = a == b

-- Sample property, Function is not correct yet

isIdempotent :: Set Int -> Set Int -> Bool
isIdempotent a b = unionSet a b == a

-- Test function with random generator

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

instance (Integral a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return (Set [a, b])

setInt' :: Set Int -> Set Int
setInt' i = i

quickCheckSetVerbose = verboseCheck (\ s -> s == setInt' (s) )

quickCheckSet = quickCheckResult (\ s -> s == setInt' (s) )