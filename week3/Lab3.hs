module Lab3 where
 
import Data.List
import System.Random


--
-- Determines whether a given formula is a contradiction
--
contradiction :: Form -> Bool
contradiction f = not.any (\ v -> evl v f) (allVals f)

tautology :: Form -> Bool
tautology = all (\ v -> evl v f) (allVals f)

