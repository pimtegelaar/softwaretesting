module Lab4_7 where

import Data.List
import SetOrd
import System.Random
import Test.QuickCheck
import Control.Monad
import Lab4_5
import Lab4_6

-- Time Spent: 6 hours

--
-- Determines whether a given relation is symmetric.
--
isSymmetric :: Ord a => Rel a -> Bool
isSymmetric [] = True
isSymmetric r = all (\(x,y) -> elem (y,x) r) r

--
-- Determines whether a given relation is asymmetric.
--
isAsymmetric :: Ord a => Rel a -> Bool
isAsymmetric [] = True
isAsymmetric r = not $ any (\(x,y) -> elem (y,x) r) r

--
-- Determines whether a given relation is transitive.
-- (Taken from the book, page 175)
--
isTransitive :: Ord a => Rel a -> Bool
isTransitive [] = True
isTransitive r = and [trans pair r | pair <- r] where 
                 trans (x,y) r = and [ elem (x,v) r | (u,v) <- r, u == y ]

-- Implemented generator for Symmetric and Transitive Closure testing
                 
randomList :: IO [Int]
randomList = randomRs (-10, 10) `fmap` newStdGen

pairs :: IO [(Int, Int)]
pairs = liftM2 zip randomList randomList

getNPairs :: Int -> IO [(Int, Int)]
getNPairs n = take n `fmap` pairs

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

-- Testable properties
--
-- The intersection of two transitive relations is transitive.
-- A transitive relation is asymmetric if and only if it is irreflexive.
-- The converse of a transitive relation is always transitive: e.g. knowing that "is a subset of" is transitive and "is a superset of" is its converse, we can conclude that the latter is transitive as well.
-- By definition, a relation cannot be both symmetric and asymmetric