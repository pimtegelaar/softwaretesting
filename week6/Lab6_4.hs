module Lab6_4 where

import System.Random
import Lecture6
import Lab6_1
import Lab6_3

-- Test Fermat against list of composites, stop when composite is identified as prime.
main :: Int -> [Integer] -> IO Integer
main _ [] = return 0
main k (c:cs) = do p <- prime_tests_F k c
                   if p
                     then return c
                   else
                     do main k cs

-- With k = 1, k = 2, k = 3
tenComposites1 = main 1 (take 10 composites)
tenComposites2 = main 2 (take 10 composites)
tenComposites3 = main 3 (take 10 composites)
tenComposites100 = main 100 (take 10 composites)

{--
 Findings:
 
 The least faulty composite number that CAN be found is 4. However,
 it is not always found in this situation. The function
 prime_tests_F does not always give the same output for composites
 due to the RandomRIO. By trying only once (k = 1), the test may
 give unreliable results. By trying with (much) more candidates
 the results become more reliable.
--} 