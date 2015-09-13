module Iban

where 

import Data.List
import Data.Char

-- Remove spaces from a String

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces [x] = [x]
removeSpaces (x:xs) = filter (/=' ') (x:xs)

-- Move first four characters to end of String
-- Invalid output when x > 1 and x < 4

moveCharacters :: String -> String
moveCharacters [] = []
moveCharacters [x] = [x]
moveCharacters (x:xs) = drop 4 ((x:xs) ++ take 4 (x:xs))

-- Convert Char to Int via conversion table
-- Throw error when illegal character is used (e.g. %)
alphaToNumeric :: Char -> Int
alphaToNumeric c
   | isDigit c = read ([c])
   | not (isAlpha c) = error ("invalid characters used " ++ show c)
   | otherwise = ord (c) - 55

-- Convert non-numeric characters to numbers 

convertCharacters :: String -> String
convertCharacters [] = []
convertCharacters (x:xs) = show ( alphaToNumeric (x)) ++ convertCharacters (xs)

-- Prepare String for IBAN check

convertToIBAN :: String ->  Integer
convertToIBAN [] = 0
convertToIBAN [x] = 0
convertToIBAN (x:xs) = read (convertCharacters (moveCharacters (removeSpaces (x:xs)))) :: Integer

-- Verify Integer against MOD 97

checkMOD97 :: Integer -> Bool
checkMOD97 x = x `mod` 97 == 1

-- Verify whether String is valid IBAN value

iban :: String -> Bool
iban [] = False
iban [x] = False
iban (x:xs) = checkMOD97 (convertToIBAN (x:xs))
