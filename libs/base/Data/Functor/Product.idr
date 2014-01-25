module Data.Functor.Product

%access public
%default total

data Product : (Type -> Type) -> (Type -> Type) -> Type -> Type where
  pair : {f : Type -> Type} -> (f a) -> (g a) -> Product f g a


instance (Functor f, Functor g) => Functor (Product f g) where
  map x (pair fa ga) = pair (map x fa) (map x ga)

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure x = pair (pure x) (pure x)
  (pair fx gx) <$> (pair fa ga) = pair (fx <$> fa) (gx <$> ga)

instance (Alternative f, Alternative g) => Alternative (Product f g) where
  empty = pair empty empty
  (pair fa ga) <|> (pair fb gb) = pair (fa <|> fb) (ga <|> gb)

instance (Monad f, Monad g) => Monad (Product f g) where
  (pair fa ga) >>= x = pair (fa >>= fstP . x) (ga >>= sndP . x)
  where
    fstP (pair a _) = a
    sndP (pair _ b) = b

