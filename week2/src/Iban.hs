module Iban where

import Data.List
import Data.Char

-- Approximate time spend on this exercise: 5 hours

-- Remove spaces from a String

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs) = filter (/=' ') (x:xs)

-- Move first four characters to end of String

moveCharacters :: String -> String
moveCharacters [] = []
moveCharacters (x:xs) = drop 4 ((x:xs) ++ take 4 (x:xs))

-- Convert Char to Int via conversion table
-- Throw error when illegal character is used (e.g. %)
alphaToNumeric :: Char -> Int
alphaToNumeric c
   | isDigit c = read [c]
   | not (isAlpha c) = error ("invalid characters used " ++ show c)
   | otherwise = ord c - 55

-- Convert non-numeric characters to numbers 

convertCharacters :: String -> String
convertCharacters = foldr ((++) . show . alphaToNumeric) []

-- Prepare String for IBAN check

convertToIban :: String ->  Integer
convertToIban [] = 0
convertToIban (x:xs) = read (convertCharacters (moveCharacters (removeSpaces (x:xs)))) :: Integer

-- Verify Integer against MOD 97

checkMOD97 :: Integer -> Bool
checkMOD97 x = x `mod` 97 == 1

-- Verify whether String is valid IBAN value

iban :: String -> Bool
iban [] = False
iban [x] = False
iban (x:xs) = checkMOD97 (convertToIban (x:xs))
