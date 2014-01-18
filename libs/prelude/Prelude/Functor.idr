module Prelude.Functor

import Prelude.Basics


class Functor (f : Type -> Type) where
    map : (a -> b) -> f a -> f b

instance Functor (\a => c -> a) where
    map f g = f . g

class CoFunctor (f : Type -> Type) where
    map' : f b -> f a -> b -> a


class Contravariant (f : Type -> Type) where
    contramap : (a -> b) -> f b -> f a

instance Contravariant (\b => b -> c) where
    contramap f g = g . f


class CoContravariant (f : Type -> Type) where
    contramap' : f a -> f b -> b -> a




