{-# LANGUAGE TypeOperators #-}

infixr |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show)

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  foldr :: (Foldable f, Foldable g) => (a -> b -> b) -> b -> (|.|) f g a -> b
  foldr f ini (Cmps composition) = let h cmps ini = foldr f ini cmps in foldr h ini composition
