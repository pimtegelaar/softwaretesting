module Lab6_6_2 where

import System.Random
import Lecture6

testMillerRabin :: Int -> [Integer] -> IO ()
testMillerRabin _ [] = print "Succeeded"
testMillerRabin k (p:ps) = do m <- primeMR k (2^p -1)
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
 
 The speed seems to be caused by the modular exponentiation.
 When we replace the method in lecture 6 with our alternative, we go up to 4423, before it starts to slow down:
 
 "Mersenne: 2"
 "Mersenne: 3"
 "Mersenne: 5"
 "Mersenne: 7"
 "Mersenne: 13"
 "Mersenne: 17"
 "Mersenne: 19"
 "Mersenne: 31"
 "Mersenne: 61"
 "Mersenne: 89"
 "Mersenne: 107"
 "Mersenne: 127"
 "Mersenne: 521"
 "Mersenne: 607"
 "Mersenne: 1279"
 "Mersenne: 2203"
 "Mersenne: 2281"
 "Mersenne: 3217"
 "Mersenne: 4253"
 "Mersenne: 4423"

--} 