module Lab4_7 where

import Data.List
import SetOrd
import System.Random
import Test.QuickCheck
import Control.Monad
import Lab4_5
import Lab4_6

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

getRandomTuple :: IO (Int, Int)
getRandomTuple = liftM2 (,) (randomRIO (1,10000)) (randomRIO (1, 10000))

testRel :: Int -> Int -> (Rel a -> Rel a)
                      -> (Rel a -> Bool) -> IO ()
testRel k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- (getRandomInt 10,getRandomInt 10)
                  if r (f xs) then
                    do print ("pass on: " ++ show xs)
                       testRel (k+1) n f r
                  else error ("failed test on: " ++ show xs)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

--randomFlip :: Int -> IO Int
--randomFlip x = do 
--  b <- getRandomInt 1
--  if b==0 then return x else return (-x)

--genIntList :: IO [Int]
--genIntList = do 
--  k <- getRandomInt 20
--  n <- getRandomInt 10
--  getIntL k n

--getIntL :: Int -> Int -> IO [Int]
--getIntL _ 0 = return []
--getIntL k n = do 
--  x <- (getRandomTuple k k)
--  y <- randomFlip x
--  xs <- getIntL k (n-1)
--  return (x:xs)
                                 

-- Testable properties
--
-- The intersection of two transitive relations is transitive.
-- A transitive relation is asymmetric if and only if it is irreflexive.
-- The converse of a transitive relation is always transitive: e.g. knowing that "is a subset of" is transitive and "is a superset of" is its converse, we can conclude that the latter is transitive as well.
-- By definition, a relation cannot be both symmetric and asymmetric