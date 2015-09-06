--
-- Crime Scene Investigation
--

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)
           
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

accusers :: Boy -> [Boy]
accusers b = [x | x <- boys, says x b]

guilty :: [Boy] -- Should return 1 element
guilty = [x | x <- boys, length (accusers x) == 3]
 
honest :: [Boy] -- Should return 3 elements
honest = accusers (head guilty)
