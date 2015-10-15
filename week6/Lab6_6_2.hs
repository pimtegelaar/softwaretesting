module Lab6_6_2 where

import System.Random
import Lecture6
import Lab6_1
import Lab6_3
import Lab6_5

testMillerRabin :: Int -> [Integer] -> IO ()
testMillerRabin _ [] = print "Succeeded"
testMillerRabin k (p:ps) = do m <- primeMR k (2^p-1)
                              if m then 
                                do 
                                  print ("Mersenne: " ++ show p)
                                  testMillerRabin k ps
                              else
                                testMillerRabin k ps

testMersennePrimes = testMillerRabin 1 (take 10000 primes)
                                
{--
 Findings:
 
 At my client this works for first Mersenne Primes, but
 it does not pass 19. Will test further later on.

--} 