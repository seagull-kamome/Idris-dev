module Prelude.Bifunctor

import Prelude.Basics
import Prelude.Functor

class Bifunctor (f : Type -> Type -> Type) where
    bimap : (a -> b) -> (c -> d) -> f a c -> f b d
    --bimap f g = first f . second g
    --first : (a -> b) -> f a c -> f b c
    --first f = bimap f id
    --second : (c -> d) -> f a c -> f a d
    --second = bimap id

--instance Bifunctor (\a, b => (a, b)) where
--    bimap f g (x, y) = (f x, g y)

class CoBifunctor (f : Type -> Type -> Type) where
    bimap' : f a c -> f b d -> (d -> c) -> (b -> a)

class Profunctor (f : Type -> Type -> Type) where
    dimap : (a -> b) -> (c -> d) -> f b c -> f a d

class CoProfunctor (f : Type -> Type -> Type) where
    dimap' : f a d -> f b c -> (d -> c) -> (b -> a)


data Flip : (Type -> Type -> Type) -> Type -> Type -> Type where
  unFlip : {f : Type -> Type -> Type} -> f b a -> Flip f a b
instance Bifunctor f => Bifunctor (Flip f) where
  bimap f g (unFlip x) = unFlip $ bimap g f x

data Crown : (Type -> Type) -> Type -> Type -> Type where
  unCrown : {f : Type -> Type} -> f a -> Crown f a b
instance Functor f => Bifunctor (Crown f) where
  bimap f _ (unCrown x) = unCrown $ map f x

data Joker : (Type -> Type) -> Type -> Type -> Type where
  unJoker : {f : Type -> Type} -> f b -> Joker f a b
instance Functor f => Bifunctor (Joker f) where
  bimap _ g (unJoker x) = unJoker $ map g x




