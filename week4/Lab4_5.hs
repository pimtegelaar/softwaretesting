module Lab4_5 where

import Data.List
import SetOrd

-- Time Spent: 1 hour

type Rel a = [(a,a)]

--
-- Gives the symmetric closure for a given relation.
--
symClos :: Ord a => Rel a -> Rel a
symClos r = union (nub r) [ (x,y) | (y,x) <- nub r]