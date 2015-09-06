-- 
-- Software Testing - Week 1
-- Erik Verhoofstad
-- 06-09-2015
--

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (x, xs) = toRevDigits x == xs

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
             [(123,[3,2,1]), (5,[5]), (0,[])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (input, expected) = doubleEveryOther input == expected

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
               [ 
                  ([], []),
                  ([4], [4]),
                  ([3,5], [3,10]),
                  ([3,6,7,8], [3,12,7,16])
               ]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (input, expected) = sumDigits input == expected

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits 
               [
                   ([], 0),
                   ([5], 5),
                   ([1,3,7], 11),
                   ([1,-1], 0),
                   ([10,5,18,4], 19)
               ]
           ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (input, expected) = luhn input == expected 

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
               [
                   (5594589764218858, True),
                   (1234567898765432, False)
               ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  ]
