module Lab2 where
 
import Data.List
import System.Random

-- Testing Functions

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n
  
getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <- getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

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

-- Exercise 2

-- Helper

quicksort :: Ord a => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) = 
   quicksort [ a | a <- xs, a <= x ]  
   ++ [x]
   ++ quicksort [ a | a <- xs, a > x ]

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation a b = if quicksort a == quicksort b then True else False