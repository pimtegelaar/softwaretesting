module Lab6_1 where

import Data.Bits
import Data.Char
import Numeric

binary :: Integer -> String
binary x = (showIntAtBase 2 intToDigit x "")

powers :: String -> [Integer]
powers [] = []
powers (x:xs) = if x == '1' then (2 ^ (length xs) : powers xs) else powers xs

exM2 :: Integer -> Integer -> Integer -> Integer
exM2 x e m = (exMs x (powers (binary e)) m) `mod` m

exMs :: Integer -> [Integer] -> Integer -> Integer
exMs x [] m = 1
exMs x (e:es) m = (expo x e m) * (exMs x es m)

expo :: Integer -> Integer -> Integer -> Integer
expo x 1 m = ms x m
expo x 2 m = ms x m
expo x e m = expo (ms x m) (shiftR e 1) m

ms :: Integer -> Integer -> Integer
ms x m = x*x `mod` m


-- Alternative method
exM' :: Integer -> Integer -> Integer -> Integer
exM' b 0 m = 1
exM' b e m = t * exM' ((b * b) `mod` m) (shiftR e 1) m `mod` m
           where t = if testBit e 0 then b `mod` m else 1
