module Prelude.Biapplicative

import Prelude.Basics
import Prelude.Bifunctor

infixl 4 <<$>>
infixl 4 <<$
infixl 4 $>>
infixl 4 <<$$>>

class Bifunctor f => Biapplicative f where
  bipure : a -> b -> f a b
  (<<$>>) : f (a -> b) (c -> d) -> f a c -> f b d
  ($>>) : f a b -> f c d -> f c d
  a $>> b = bimap (const id) (const id) `fmap` a <<*>> b


