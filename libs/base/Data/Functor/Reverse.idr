module Data.Functor.Reverse

%access public
%default total

record Reverse : (Type -> Type) -> Type -> Type where
  reverse_intro : {f : Type -> Type} -> (reversed : f a) -> Reverse f a


instance Functor  f => Functor (Reverse f) where
  map g (reverse_intro x) = reverse_intro $ map g x

instance Applicative f => Applicative (Reverse f) where
  pure = reverse_intro . pure
  (reverse_intro x) <$> (reverse_intro y) = reverse_intro (x <$> y)

instance Monad f => Monad (Reverse f) where
  (reverse_intro x) >>= g = reverse_intro $ x >>= fa . g where
    fa (reverse_intro x) = x

instance Foldable f => Foldable (Reverse f) where
  foldr x acc (reverse_intro y) = foldr x acc y

instance (Functor f, Traversable f) => Traversable (Reverse f) where
  traverse x (reverse_intro y) = map reverse_intro $ traverse x y