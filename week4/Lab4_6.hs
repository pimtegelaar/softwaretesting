module Lab4_6 where

import Data.List
import SetOrd
import Lab4_5

-- Time Spent: 2 hours

infixr 5 @@

--
-- Gives the composition (R o S) of two given relations.
--
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

--
-- Gives the transitive closure (R+) for a given relation
--
trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos (x:xs) = r ++ (r @@ r)