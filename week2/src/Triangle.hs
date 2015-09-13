module Triangle where

import Data.List
import System.Random

data Shape = NoTriangle | Equilateral 
            | Isosceles  | Rectangular | Other deriving (Eq,Show)


triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | x <= 0 || y <= 0 || z <= 0 = NoTriangle
               | x >= y + z || y >= x + z || z >= x + y = NoTriangle
               | x == y && y == z = Equilateral
               | x == y || x == z || y == z = Isosceles
               | x^2 + y^2 == z^2 = Rectangular
               | y^2 + z^2 == x^2 = Rectangular
               | x^2 + z^2 == y^2 = Rectangular
               | otherwise = Other
