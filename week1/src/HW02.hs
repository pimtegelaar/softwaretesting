{-# OPTIONS_GHC -Wall #-}
module HW02 where

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

says :: Boy -> Boy -> Bool
says b1 b2 | b1 == Matthew && b2 == Carl = False
           | b1 == Matthew && b2 == Matthew = False
           | b1 == Peter && b2 == Matthew = True
           | b1 == Peter && b2 == Jack = True
           | b1 == Jack && b2 == Matthew = True
           | otherwise = False

accusers :: Boy -> [Boy]
accusers b1 = [b1]

guilty :: Boy -> [Boy]
guilty b1 = [b1]

honest :: Boy -> [Boy]
honest b1 = [b1]