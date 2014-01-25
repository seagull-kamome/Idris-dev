module Data.Functor.Compose

%access public
%default total

data Compose : (Type -> Type) -> (Type -> Type) -> Type -> Type where
  compose : {f,g  : Type -> Type} -> (getCompose : f (g a)) -> Compose f g a

instance (Functor f, Functor g) => Functor (Compose f g) where
  map x (compose y) = compose (map (map x) y)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = compose .pure . pure
  (compose x) <$> (compose y) = compose (pure (<$>) <$> x <$> y)


-- Avoid type checker confusion
private
alt : (Alternative f) => {g:Type->Type} -> f (g a) -> f (g a) -> f (g a)
alt = (<|>)

instance (Alternative f, Alternative g) => Alternative (Compose f g) where
  empty = compose (the (f _) empty)
  (compose x) <|> (compose y) = compose (alt x y)


--instance (Foldable f, Foldable g) => Foldable (Compose f g) where
--  foldr x acc (compose y) = foldr (foldr x acc) y
--instance (Traversable f, Traversable g) => Traversable (Compose f g) where
--  traverse x (compose y) = pure compose <$> traverse (traverse x) y
