module Lab4_7 where

import Data.List
import SetOrd
import System.Random
import Test.QuickCheck
import Control.Monad
import Lab4_5
import Lab4_6

-- Time Spent: 7 hours

--
-- Determines whether a given relation is symmetric.
--
isSymmetric :: Ord a => Rel a -> Bool
isSymmetric [] = False
isSymmetric r = all (\(x,y) -> elem (y,x) r) r

--
-- Determines whether a given relation is asymmetric.
--
isAsymmetric :: Ord a => Rel a -> Bool
isAsymmetric [] = False
isAsymmetric r = not $ any (\(x,y) -> elem (y,x) r) r

--
-- Determines whether a given relation is transitive.
-- (Taken from the book, page 175)
--
isTransitive :: Ord a => Rel a -> Bool
isTransitive [] = False
isTransitive r = and [trans pair r | pair <- r] where 
                 trans (x,y) r = and [ elem (x,v) r | (u,v) <- r, u == y ]

-- Implemented generator for Symmetric and Transitive Closure testing
                 
randomList :: IO [Int]
randomList = randomRs (-10, 10) `fmap` newStdGen

pairs :: IO [(Int, Int)]
pairs = liftM2 zip randomList randomList

getNPairs :: Int -> IO [(Int, Int)]
getNPairs n = take n `fmap` pairs

-- Perform tests with a fixed amount of tuples. Was necessary to do this
-- testing on Int, due to method of generating tuples. Should/can be altered
-- for other input as well.

testRel :: Int -> Int -> (Rel Int -> Rel Int)
                      -> (Rel Int -> Bool) -> IO ()
testRel k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- (getNPairs 8)
                  if r (f xs) then
                    do print ("pass on: " ++ show xs)
                       testRel (k+1) n f r
                  else error ("failed test on: " ++ show xs)

testRels :: (Rel Int -> Rel Int) -> (Rel Int -> Bool) -> IO ()
testRels f p = testRel 1 10 f p

-- QuickCheck

-- Verify that the Symmetric Closure is the same as the
-- Symmetric Closure of the Symmetric Closure
propSymClos :: Rel Int -> Rel Int -> Bool
propSymClos xs ys = symClos xs == symClos (symClos xs)

-- By definition, a relation cannot be both symmetric and asymmetric
propSymNotAsym :: Rel Int -> Bool
propSymNotAsym xs = not (isAsymmetric (symClos xs))

-- Similar, but show length of test cases
propSymNotAsym' :: Rel Int -> Property
propSymNotAsym' xs = collect (length xs) $ not (isAsymmetric (symClos xs))

-- Verify that the Transitive Closure is the same as the
-- Transitive Closure of the Transitive Closure
propTrClos :: Rel Int -> Rel Int -> Bool
propTrClos xs ys = trClos xs == trClos (trClos xs)

main = quickCheck propSymNotAsym'