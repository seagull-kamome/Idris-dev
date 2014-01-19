module Control.Monad.Indexed

import Prelude.Basics

%access public
%default total

class IndexedFunctor (f : Type -> Type -> Type -> Type) where
  ixmap : (a -> b) -> f i j a -> f i j b

class IndexedFunctor f => IndexedApplicative (f : Type -> Type -> Type -> Type) where
  ixpure : a -> f i i a
  ixapply : f i i (a -> b) -> f i i a -> f i i b

class IndexedApplicative f => IndexedMonad (f : Type -> Type -> Type -> Type) where
  ixbind : (a -> f i j b) -> f k j a -> f k j b

ixreturn : IndexedMonad f => a -> f i i a
ixreturn = ixpure

ixflatten : IndexedMonad f => f i j (f i j a) -> f i j a
ixflatten a = id `ixbind` a

