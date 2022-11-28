newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap f arr2 = Arr2 $ \e1 e2 -> let a = getArr2 arr2 e1 e2 in f a

instance Functor (Arr3 e1 e2 e3) where
  fmap f arr3 = Arr3 $ \e1 e2 e3 -> let a = getArr3 arr3 e1 e2 e3 in f a
