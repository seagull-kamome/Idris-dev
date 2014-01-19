module Prelude.Propositional

import Prelude.Either
import Prelude.Basics

infixl 3 /\, \/


(\/): Type -> Type -> Type
x \/ y = Either x y

(/\) : Type -> Type -> Type
x /\ y = (x, y)



exfalso : a -> Not a -> b
exfalso a neg = FalseElim (neg a)  

or_introL: a -> a \/ b
or_introL = Left
or_introR : b -> a \/ b
or_introR = Right

or_elim : a \/ b -> (a -> c) -> (b -> c) -> c
or_elim = either

or_assocL : a \/ (b \/ c) -> (a \/ b) \/ c
or_assocL (Left a) = Left (Left a)
or_assocL (Right (Left b)) = Left (Right b)
or_assocL (Right (Right c)) = Right c

or_assocR : (a \/ b) \/ c -> a \/ (b \/ c)
or_assocR (Left (Left a)) = Left a
or_assocR (Left (Right b)) = Right (Left b)
or_assocR (Right c) = Right (Right c)

and_intro : a -> b -> a /\ b
and_intro x y = (x, y)

and_elimL : a /\ b -> a
and_elimL = fst
and_elimR : a /\ b -> b
and_elimR = snd

and_assocL : a /\ (b /\ c) -> (a /\ b) /\ c
and_assocL (a, (b,c)) = ((a,b),c)

and_assocR : (a /\ b) /\ c -> a /\ (b /\ c)
and_assocR ((a,b),c) = (a,(b,c))


