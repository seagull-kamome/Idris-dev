module Data.Functor.Constant


%access public
%default total


data Constant : Type -> Type -> Type where
  constant : (getConstant : a) -> Constant a b

instance Functor (Constant a) where
  map f (constant x) = constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = constant neutral
  (constant x) <$> (constant y) = constant (x <+> y)
  