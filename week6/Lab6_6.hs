module Lab6_6 where

import System.Random
import Lecture6
import Lab6_1
import Lab6_3
import Lab6_4
import Lab6_5

testMillerRabin :: Int -> [Integer] -> IO ()
testMillerRabin _ [] = print "Succeeded"
testMillerRabin k (c:cs) = do p <- primeMR k c
                              if p then print ("False - Failed for " ++ show c)
                              else
                                testMillerRabin k cs

testMRwithComposites1 = testMillerRabin 1 (take 50 composites)
testMRwithComposites5 = testMillerRabin 5 (take 50 composites)
testMRwithComposites10 = testMillerRabin 10 (take 50 composites)

testMRwithCarmichael1 = testMillerRabin 1 (take 5 carmichael)
testMRwithCarmichael5 = testMillerRabin 5 (take 5 carmichael)
testMRwithCarmichael10 = testMillerRabin 10 (take 5 carmichael)
                                
{--
 Findings:
 
 For Composites the test seems to work properly, especially with higher
 values for k.
 
 For Carmichael numbers the test has problems. On the moment the
 test does not finish when giving 5 Carmichael values. Still need
 to figure out what goes wrong.

--} 