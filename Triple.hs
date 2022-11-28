data Triple a = Tr a a a deriving (Eq, Show)

instance Functor Triple where
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
  pure a = Tr a a a
  (Tr xFunc yFunc zFunc) <*> (Tr x y z) = Tr (xFunc x) (yFunc y) (zFunc z)
