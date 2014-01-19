module Data.Hashable

%access public
%default total

class Hashable a where
  hash : a -> Integer

instance Hashable Int where hash = cast
instance Hashable Integer where hash = id
instance Hashable Char where hash = cast . the Int . cast
instance Hashable Float where hash = cast . the Int .cast
instance Hashable Nat where hash = cast
instance Hashable () where hash _ = 0
instance (Hashable a, Hashable b) => Hashable (Either a b) where
  hash (Left x) = hash x
  hash (Right x) = hash x
instance (Hashable a, Hashable b) => Hashable (a,b) where
  hash (x,y) = hash x + hash y
instance Hashable a => Hashable (List a) where
  hash = sum . map hash


