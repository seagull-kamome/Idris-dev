module Data.Functor.Identity


%access public
%default total

data Identity : Type -> Type where
  identity : (runIdentity : a) -> Identity a

instance Functor Identity where
  map f (identity x) = identity $ f x

instance Applicative Identity where
  pure = identity
  (identity f) <$> (identity x) = identity $ f x

instance Monad Identity where
  (identity x) >>= f = f x

instance Foldable Identity where
  foldr f acc (identity x) = f x acc

instance Traversable Identity where
  traverse f (identity x) = pure identity <$> f x

