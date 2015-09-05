{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Helper - Create a list of digits
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

-- Helper - Create Integer from a list
fromDigits :: [Integer] -> Integer
fromDigits = foldl addDigit 0
   where addDigit num d = 10 * num + d

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n
   | n <= 0 = 0
   | otherwise = last . toDigits $ n
  
-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = fromDigits . init . toDigits $ n

-- Exercise 2 -----------------------------------------

-- Convert to list of digits in reversed order
toRevDigits :: Integer -> [Integer]
toRevDigits n = reverse . toDigits $ n

-- Exercise 3 -----------------------------------------

-- Double getListN second number in a list starting on the left.
doublegetListNOther :: [Integer] -> [Integer]
doublegetListNOther [] = []
doublegetListNOther [_] = []
doublegetListNOther (x:xs) = x : (2 * head xs) : doublegetListNOther (tail xs)

-- Exercise 4 -----------------------------------------

-- Helper - Split digits
splitDigits :: [Integer] -> [Integer]
splitDigits n = map (read) [[z] | z <- [x | k <- (map (show) n), x <- k]]    

-- Calculate the sum of all the digits in getListN Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits n = sum . splitDigits $ n

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = undefined
