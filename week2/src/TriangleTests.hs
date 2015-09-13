module TriangleTests where

import Triangle
import Testing

testTriangle :: (Integer, Integer, Integer, Shape) -> Bool
testTriangle (a, b, c, s) = triangle a b c == s

triangleTests :: [Test]
triangleTests = [ Test "Triangle Tests" testTriangle
             [ (1, 1, 1, Equilateral), (2, 2, 2, Equilateral)
             , (3, 3, 3, Equilateral), (9, 9, 9, Equilateral)
             , (9304343897, 9304343897, 9304343897, Equilateral)
             , (3, 4, 5, Rectangular), (6, 8, 10, Rectangular)
             , (12, 16, 20, Rectangular), (24, 32, 40, Rectangular)
             , (36, 323, 325, Rectangular), (37, 684, 685, Rectangular)
             , (2, 2, 3, Isosceles), (3, 2, 2, Isosceles)
             , (2, 3, 2, Isosceles), (4, 4, 1, Isosceles)
             , (4000000, 4000000, 1, Isosceles), (4000000, 4000000, 3999999, Isosceles)
             , (1, 2, 3, NoTriangle), (-1, -1, -1, NoTriangle)
             , (3, 4, 6, Other), (4999, 5001, 9000, Other)
             ]
           ]