module Lab3_1 where
 
import Data.List
import System.Random
import Lecture3

-- Time spent: 2 hours

--
-- Determines whether a given formula is a contradiction.
--
contradiction :: Form -> Bool
contradiction f = not $ any (\ v -> evl v f) (allVals f)

--
-- Determines whether a given formula is a tautology.
--
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

--
-- Determine whether one formula is a consequent of another formula.
--
entails :: Form -> Form -> Bool
entails f g = all (\ v -> evl v (Impl f g)) (allVals f)

--
-- Determines whether two given formulas are equivalent.
--
equiv :: Form -> Form -> Bool
equiv f g = all (\v -> evl v f == evl v g) (allVals f)
