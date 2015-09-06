module CSI where

import Data.List

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)
 
boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

says :: Boy -> Boy -> Bool
says Jack b = b /= Matthew && b /= Peter
says Arnold b = b == Matthew || b == Peter
says Carl b = b /= Arnold
says _ _ = True

accusers :: Boy -> [Boy]
accusers Matthew = delete Matthew(delete Carl boys)
accusers Peter = [Matthew, Jack]
accusers _ = boys

accuses :: Boy ->  Boy -> Bool
accuses boy culprit = culprit `elem` accusers boy

notlying :: Boy -> Boy -> Bool
notlying boy culprit = accuses boy culprit

guilty, honest :: [Boy]
guilty = undefined
honest = undefined
