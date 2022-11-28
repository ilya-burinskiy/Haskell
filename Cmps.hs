{-# LANGUAGE ExplicitNamespaces #-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps x) = Cmps $ fmap (fmap h) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure
  (Cmps h) <*> (Cmps x) = Cmps $ fmap (<*>) h <*> x 

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 (Cmps x) = fmap getCmps x

unCmps4 :: (Functor f1, Functor f2) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 (Cmps x) = fmap (\(Cmps x) -> fmap getCmps x) x
