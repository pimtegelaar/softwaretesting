module Lab6_5 where

import System.Random
import Lecture6
import Lab6_1
import Lab6_3
import Lab6_4

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]
      
      
carmichael1 = main 1 [carmichael!!0]
carmichael2 = main 2 [carmichael!!0]
carmichael3 = main 3 [carmichael!!0]
carmichael5 = main 5 [carmichael!!0]
carmichael10 = main 10 [carmichael!!0]
carmichael20 = main 10 [carmichael!!0]
carmichael100 = main 10 [carmichael!!0]

          
highComposites :: [Integer]
highComposites = [ x | x <- [294400..], not (isPrime x) ]

highComposites100 = main 100 (take 10 highComposites)
highComposites75 = main 75 (take 10 highComposites)
highComposites50 = main 50 (take 10 highComposites)
highComposites25 = main 25 (take 10 highComposites)
highComposites1 = main 1 (take 10 highComposites)

{--
 Findings:
 
 Via these tests it can be showed that the Fermat test has difficulty
 with the Carmichael numbers. For a specific test on the range 294400-294410
 in many attempts the number 294409 will be marked as Prime. The higher k gets,
 the less times it is indicated. But even with k = 50, it still occurs. So
 although the chance on mistakes seems to be higher for Carmichael numbers,
 it does not seem to behave completely different. It still also happens that
 the test does give correct output. This is (up to a certain degree) random.
--} 