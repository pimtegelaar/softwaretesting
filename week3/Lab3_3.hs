module Lab3_3 where
 
import Data.List
import System.Random
import Lecture3
-- Time spent: 3 hours.
-- Yes, that's about 30 min. per line of code.

--
-- Converts any propositional formula to CNF form.
--
cnf :: Form -> Form
cnf f = cnf'( nnf ( arrowfree f))

--
-- Converts a propositional formula from NNF to CNF
--
cnf' :: Form -> Form
cnf' (Prop x) = Prop x
cnf' (Neg (Prop x)) = Neg (Prop x)
cnf' (Dsj [x,(Cnj [y,z])]) = Cnj[cnf'(Dsj[x,y]), cnf'(Dsj[x,z])]   -- Application of the distributive laws
cnf' (Dsj [Cnj [x,y],z]) = Cnj[cnf'(Dsj[x,y]), cnf'(Dsj[x,z])]     -- to 'push' the disjunctions inwards.
cnf' (Dsj fs) = Dsj (map cnf' fs)
cnf' (Cnj fs) = Cnj (map cnf' fs)