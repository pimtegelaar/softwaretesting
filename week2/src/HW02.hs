module HW02 where
 
import Data.List
import Data.Char

-- Exercise 1

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
   | a <= 0 || b <= 0 || c <= 0 = NoTriangle
   | a >= b + c || b >= a + c || c >= a + b = NoTriangle
   | a == b && b == c = Equilateral
   | a == b || b == c || a == c = Isosceles
   | a^2 + b^2 == c^2 = Rectangular
   | b^2 + c^2 == a^2 = Rectangular
   | a^2 + c^2 == b^2 = Rectangular
   | otherwise = Other

rectangular :: (Integer, Integer, Integer)
rectangular = (1,2,3)
   
-- Exercise 2

-- Helper

quicksort :: Ord a => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) = 
   quicksort [ a | a <- xs, a <= x ]  
   ++ [x]
   ++ quicksort [ a | a <- xs, a > x ]

-- First order both lists, then compare them (modified class from 'Eq' to 'Ord')

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation a b = if quicksort a == quicksort b then True else False

-- Variant 2

isPermutation' :: Eq a => [a] -> [a] -> Bool
isPermutation' [] [] = True
isPermutation' xs (y:ys) | length xs /= length (y:ys) = False
                         | otherwise = isPermutation' (delete y xs) ys

-- Exercise 3

-- Helper

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)
  
-- Variant 2

perms' :: Eq a => [a] -> [[a]]
perms' [] = [[]]
perms' xs = [ i:j | i <- xs, j <- perms' $ delete i xs ]

-- Check whether list is a derangement of other list

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement xs ys = and [ x `elem` ys && (index x xs /= index x ys) | x <- xs ] where
      index n (x:xs) | n == x = 0
                     | otherwise = 1 + index n xs

-- Generate all derangements for n-1 Int

deran :: Int -> [[Int]]
deran n = filter (\ p -> isDerangement p [0..n-1]) (perms [0..n-1])

-- Exercise 4

-- Helpers

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