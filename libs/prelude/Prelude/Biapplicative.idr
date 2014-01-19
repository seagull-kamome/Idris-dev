module Prelude.Biapplicative

import Prelude.Basics
import Prelude.Bifunctor

infixl 2 <<$>>

class Bifunctor f => Biapplicative (f : Type -> Type -> Type) where
  bipure : a -> b -> f a b
  (<<$>>) : f (a -> b) (c -> d) -> f a c -> f b d

infixl 2 <<$
($>>) : Biapplicative f => f a b -> f c d -> f c d
a $>> b = bimap (const id) (const id) a <<$>> b


