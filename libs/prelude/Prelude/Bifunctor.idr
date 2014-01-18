module Prelude.Bifunctor

import Prelude.Basics

class Bifunctor (f : Type -> Type -> Type) where
    bimap : (a -> b) -> (c -> d) -> f a c -> f b d
    bimap f g = first f . second g
    first : (a -> b) -> f a c -> f b c
    first f = bimap f id
    second : (c -> d) -> f a c -> f a d
    second = bimap id

--instance Bifunctor (\a, b => (a, b)) where
--    bimap f g (x, y) = (f x, g y)

class CoBifunctor (f : Type -> Type -> Type) where
    bimap' : f a c -> f b d -> (d -> c) -> (b -> a)

class Profunctor (f : Type -> Type -> Type) where
    dimap : (a -> b) -> (c -> d) -> f b c -> f a d

class CoProfunctor (f : Type -> Type -> Type) where
    dimap' : f a d -> f b c -> (d -> c) -> (b -> a)

