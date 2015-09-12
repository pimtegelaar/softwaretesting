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
toRevDigits 0 = [] -- Base-case for 0 otherwise it will divide through 10 indefinitly
toRevDigits x = lastDigit x : toRevDigits (dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:ys) = x : (y*2) : doubleEveryOther ys

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.

digits :: Integer -> [Integer]
digits = map (read . (:[])) . show

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits x = sum (splitDigits x)

splitDigits :: [Integer] -> [Integer]
splitDigits = foldr ((++) . digits) []


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = (lastDigit . sumDigits . doubleEveryOther . toRevDigits $ x) == 0
