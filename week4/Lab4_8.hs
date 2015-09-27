module Lab4_8 where

import Data.List
import SetOrd
import Lab4_5
import Lab4_6

--
-- No, they are not the same. For example, if you have a relation R containing {(1,2)},
--
-- the symmetric closure of the transitive closure of R will be:
-- 
-- {(1,2)} -- transitive closure --> {(1,2)} -- symmetric closure --> {(1,2),(2,1)}
--
-- while the transitive closure of the symmetric closureof R will be:
--
-- {(1,2)} -- symmetric closure --> {(1,2),(2,1)} -- transitive closure --> {(1,1),(1,2),(2,1),(2,2)}
--

relAreEqual :: Ord a => Rel a -> Rel a -> Bool
relAreEqual r s = relAreEqual' (nub r) (nub s)

relAreEqual' :: Ord a => Rel a -> Rel a -> Bool
relAreEqual' [] [] = True
relAreEqual' [] _ = False
relAreEqual' _ [] = False
relAreEqual' (x:xs) ys = elem x ys && relAreEqual' xs (delete x ys)

tranSymClos :: Ord a => Rel a -> Rel a
tranSymClos r = trClos (symClos r)

symTranClos :: Ord a => Rel a -> Rel a
symTranClos r = symClos (trClos r)

counterExample :: Bool
counterExample = not $ relAreEqual (tranSymClos [(1,2)]) (symTranClos [(1,2)])