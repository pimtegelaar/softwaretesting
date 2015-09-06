module CSI where

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)
 
boys = [Matthew, Peter, Jack, Arnold, Carl]

says :: Boy -> Boy -> Bool
says = undefined

accusers :: Boy -> [Boy]
accusers = undefined

guilty, honest :: [Boy]
guilty = undefined