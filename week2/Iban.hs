--
-- Software Testing - Week 2
-- Erik Verhoofstad
--
-- Time spent:
--    functions: 2 Hours
--
module Iban

where 

import Data.List
import Data.Char

rearrange :: String -> String
rearrange [] = []
rearrange xs = drop 4 xs ++ take 4 xs

convert :: String -> [Int]
convert [] = []
convert (x:xs) = convertChar x : convert xs where
   convertChar x | isDigit x = ord x - 48
                 | isAlpha x = ord (toUpper x) - 55
                 | otherwise = 0

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs

iban :: String -> Bool
iban [] = False
iban xs = mod (sumDigits( convert (rearrange xs))) 97 == 0
