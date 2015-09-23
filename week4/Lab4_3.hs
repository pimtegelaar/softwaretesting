module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

--
-- Returns the intersection of two given sets.
--
intersectSet :: Ord a => Set a -> Set a -> Set a
intersectSet (Set []) (Set ys) = Set []
intersectSet (Set (x:xs)) (Set ys) | inSet x (Set ys) = insertSet x (intersectSet (Set xs) (Set ys))
                                   | otherwise        = intersectSet (Set xs) (Set ys)

--
-- Returns the difference of two given sets.
--
differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet (Set []) (Set _)  = Set []
differenceSet (Set xs) (Set []) = Set xs
differenceSet (Set (x:xs)) (Set ys) | inSet x (Set ys) = differenceSet (Set xs) (Set ys)
                                    | otherwise        = insertSet x (differenceSet (Set xs) (Set ys))