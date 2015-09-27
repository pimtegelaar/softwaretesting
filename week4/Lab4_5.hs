module Lab4_5 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):xs) = nub ((x,y):(y,x):symClos xs)