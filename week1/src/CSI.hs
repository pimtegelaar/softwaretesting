{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module CSI where

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)
           
boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

says :: Boy -> Boy -> Bool
says Matthew Matthew = False
says Matthew Carl = False
says Matthew _ = True
says Peter Matthew = True
says Peter Jack = True
says Peter _ = False
says Jack x = not(says Matthew x) && not(says Peter x)
says Arnold x = says Matthew x /= says Peter x
says Carl x = not(says Arnold x)
says _ _ = False

accusers :: Boy -> [Boy]
accusers b = [x | x <- boys, says x b]

guilty :: [Boy] -- Should return 1 element
guilty = [x | x <- boys, length (accusers x) == 3]
 
honest :: [Boy] -- Should return 3 elements
honest = accusers (head guilty)

test1 :: String
test1 = "3 Boys are honest = " ++ show (length honest == 3) 

test2:: String
test2 = "1 boy is guilty = " ++ show (length guilty == 1)

test3 :: String
test3 = "The culprit is: " ++ show (Jack `elem` guilty)

test :: IO ()
test = putStr(test1 ++ "\n" ++  test2 ++ "\n" ++ test3 ++ "\n")
