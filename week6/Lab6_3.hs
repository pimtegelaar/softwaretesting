module Lab6_3 where

import Lecture6

composites :: [Integer]
composites = [ x | x <- [2..], not (isPrime x) ]
