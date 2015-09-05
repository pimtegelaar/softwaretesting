{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = [0]
toRevDigits x = reverseDigits x

reverseDigits :: Integral x => x -> [x]
reverseDigits 0 = []
reverseDigits x = x `mod` 10 : reverseDigits (x `div` 10)

-- Slower version, but handles negatives
toRevDigits2 :: Integer -> [Integer]
toRevDigits2 x = digits(signum x * (read . reverse . show . abs $ x))
   
digits :: Integer -> [Integer]
digits = map (read . (:[])) . show

-- Concatenate the list of Integers back to an integer
fromDigits :: [Integer] -> Integer
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d
      
-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs) = x  : dother xs

dother :: [Integer] -> [Integer]
dother [] = []
dother (x:xs) = x * 2 : doubleEveryOther xs
-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
--sumDigits = undefined
sumDigits x = sum (splitDigits x)

splitDigits :: [Integer] -> [Integer]
splitDigits = foldr ((++) . digits) []

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = lastDigit(sumDigits(doubleEveryOther(toRevDigits x))) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
