module Lab6_2 where

import Data.Bits
import Data.Char
import Numeric
import Criterion.Main
import Lecture6
import Lab6_1
           
main = defaultMain [
  bgroup "expM" [ 
                 bench "117"  $ whnf (expM 5 117) 19
               , bench "200"  $ whnf (expM 3 200) 50
               , bench "1170"  $ whnf (expM 5 1170) 19
               , bench "2000"  $ whnf (expM 3 2000) 50
               ]
               ,
  bgroup "exM2" [ 
                 bench "117"  $ whnf (exM2 5 117) 19
               , bench "200"  $ whnf (exM2 3 200) 50
               , bench "1170"  $ whnf (exM2 5 1170) 19
               , bench "2000"  $ whnf (exM2 3 2000) 50
               ]
               ,
  bgroup "exM'" [ 
                 bench "117"  $ whnf (exM' 5 117) 19
               , bench "200"  $ whnf (exM' 3 200) 50
               , bench "1170"  $ whnf (exM' 5 1170) 19
               , bench "2000"  $ whnf (exM' 3 2000) 50
               ]
  ]
  
{--
Result of main:

Summary: The performance of expM decreases dramatically when the exponent increases.
         The new variants do better in this respect.

Details:

    benchmarking expM/117
    time                 81.79 ns   (81.49 ns .. 82.14 ns)
                         1.000 Rý   (1.000 Rý .. 1.000 Rý)
    mean                 81.63 ns   (81.43 ns .. 81.84 ns)
    std dev              659.7 ps   (516.0 ps .. 855.6 ps)
    
    benchmarking expM/200
    time                 87.27 ns   (87.01 ns .. 87.70 ns)
                         1.000 Rý   (1.000 Rý .. 1.000 Rý)
    mean                 87.63 ns   (87.21 ns .. 88.20 ns)
    std dev              1.650 ns   (1.226 ns .. 2.290 ns)
    
    benchmarking expM/1170
    time                 198.2 ns   (197.9 ns .. 198.6 ns)
                         1.000 Rý   (1.000 Rý .. 1.000 Rý)
    mean                 198.3 ns   (197.9 ns .. 198.8 ns)
    std dev              1.383 ns   (1.124 ns .. 1.699 ns)
    
    benchmarking expM/2000
    time                 220.8 ns   (220.5 ns .. 221.2 ns)
                         1.000 Rý   (1.000 Rý .. 1.000 Rý)
    mean                 221.0 ns   (220.6 ns .. 221.6 ns)
    std dev              1.583 ns   (1.193 ns .. 2.238 ns)
    
    
    
    benchmarking exM2/117
    time                 20.71 us   (20.66 us .. 20.76 us)
                         1.000 Rý   (1.000 Rý .. 1.000 Rý)
    mean                 20.74 us   (20.69 us .. 20.80 us)
    std dev              166.8 ns   (127.0 ns .. 245.0 ns)
    
    benchmarking exM2/200
    time                 18.29 us   (18.25 us .. 18.33 us)
                         1.000 Rý   (1.000 Rý .. 1.000 Rý)
    mean                 18.25 us   (18.21 us .. 18.29 us)
    std dev              133.2 ns   (112.6 ns .. 160.9 ns)
    
    benchmarking exM2/1170
    time                 24.83 us   (24.77 us .. 24.90 us)
                         1.000 Rý   (1.000 Rý .. 1.000 Rý)
    mean                 24.81 us   (24.77 us .. 24.86 us)
    std dev              153.9 ns   (130.8 ns .. 193.2 ns)
    
    benchmarking exM2/2000
    time                 44.10 us   (44.01 us .. 44.19 us)
                         1.000 Rý   (1.000 Rý .. 1.000 Rý)
    mean                 44.08 us   (44.01 us .. 44.16 us)
    std dev              240.4 ns   (206.3 ns .. 291.3 ns)
    
    

    benchmarking exM'/117
    time                 8.811 us   (8.781 us .. 8.860 us)
                         1.000 Rý   (0.999 Rý .. 1.000 Rý)
    mean                 8.810 us   (8.788 us .. 8.859 us)
    std dev              104.8 ns   (58.01 ns .. 207.7 ns)
    
    benchmarking exM'/200
    time                 9.931 us   (9.915 us .. 9.951 us)
                         1.000 Rý   (1.000 Rý .. 1.000 Rý)
    mean                 9.936 us   (9.919 us .. 9.958 us)
    std dev              68.72 ns   (54.20 ns .. 94.26 ns)
    
    benchmarking exM'/1170
    time                 13.67 us   (13.65 us .. 13.69 us)
                         1.000 Rý   (1.000 Rý .. 1.000 Rý)
    mean                 13.68 us   (13.65 us .. 13.69 us)
    std dev              76.89 ns   (62.96 ns .. 94.02 ns)
    
    benchmarking exM'/2000
    time                 13.77 us   (13.75 us .. 13.80 us)
                         1.000 Rý   (1.000 Rý .. 1.000 Rý)
    mean                 13.76 us   (13.74 us .. 13.80 us)
    std dev              90.10 ns   (74.81 ns .. 109.5 ns)

--}