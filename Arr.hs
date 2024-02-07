import Control.Monad.Trans (MonadTrans (lift))

newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}

newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap :: (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
  fmap f arr2 = Arr2 $ \e1 e2 -> let a = getArr2 arr2 e1 e2 in f a

instance Functor (Arr3 e1 e2 e3) where
  fmap :: (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b
  fmap f arr3 = Arr3 $ \e1 e2 e3 -> let a = getArr3 arr3 e1 e2 e3 in f a

instance Applicative (Arr2 e1 e2) where
  pure :: a -> Arr2 e1 e2 a
  pure x = Arr2 $ \e1 e2 -> x

  (<*>) :: Arr2 e1 e2 (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
  (<*>) fArr2 aArr2 = Arr2 $ \e1 e2 ->
    let f = getArr2 fArr2 e1 e2
        a = getArr2 aArr2 e1 e2
     in f a

instance Applicative (Arr3 e1 e2 e3) where
  pure :: a -> Arr3 e1 e2 e3 a
  pure x = Arr3 $ \e1 e2 e3 -> x

  (<*>) :: Arr3 e1 e2 e3 (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b
  (<*>) fArr3 aArr3 = Arr3 $ \e1 e2 e3 ->
    let f = getArr3 fArr3 e1 e2 e3
        a = getArr3 aArr3 e1 e2 e3
     in f a

newtype Arr2T e1 e2 m a = Arr2T {getArr2T :: e1 -> e2 -> m a}

newtype Arr3T e1 e2 e3 m a = Arr3T {getArr3T :: e1 -> e2 -> e3 -> m a}

arr2 :: (Monad m) => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T $ \e1 e2 -> return $ f e1 e2

arr3 :: (Monad m) => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T $ \e1 e2 e3 -> return $ f e1 e2 e3

instance (Functor m) => Functor (Arr2T e1 e2 m) where
  fmap :: (Functor m) => (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
  fmap f arr2' = Arr2T $ \e1 e2 ->
    let ma = getArr2T arr2' e1 e2
     in f <$> ma

instance (Functor m) => Functor (Arr3T e1 e2 e3 m) where
  fmap :: (Functor m) => (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
  fmap f arr3' = Arr3T $ \e1 e2 e3 ->
    let ma = getArr3T arr3' e1 e2 e3
     in f <$> ma

instance (Applicative m) => Applicative (Arr2T e1 e2 m) where
  pure :: (Applicative m) => a -> Arr2T e1 e2 m a
  pure x = Arr2T $ \_ _ -> pure x

  (<*>) :: Arr2T e1 e2 m (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
  (<*>) af arr2' = Arr2T $ \e1 e2 ->
    let mf = getArr2T af e1 e2
        ma = getArr2T arr2' e1 e2
     in mf <*> ma

instance MonadTrans (Arr2T e1 e2) where
  lift :: (Monad m) => m a -> Arr2T e1 e2 m a
  lift m = Arr2T $ \_ _ -> m

asks2 :: (Monad m) => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ \e1 e2 -> return (f e1 e2)

instance (Applicative m) => Applicative (Arr3T e1 e2 e3 m) where
  pure :: (Applicative m) => a -> Arr3T e1 e2 e3 m a
  pure x = Arr3T $ \_ _ _ -> pure x

  (<*>) :: (Applicative m) => Arr3T e1 e2 e3 m (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
  (<*>) af arr3' = Arr3T $ \e1 e2 e3 ->
    let mf = getArr3T af e1 e2 e3
        ma = getArr3T arr3' e1 e2 e3
     in mf <*> ma

instance (Monad m) => Monad (Arr2T e1 e2 m) where
  return :: (Monad m) => a -> Arr2T e1 e2 m a
  return = pure

  (>>=) :: (Monad m) => Arr2T e1 e2 m a -> (a -> Arr2T e1 e2 m b) -> Arr2T e1 e2 m b
  (>>=) ma k = Arr2T $ \e1 e2 -> do
    a <- getArr2T ma e1 e2
    getArr2T (k a) e1 e2

instance (Monad m) => Monad (Arr3T e1 e2 e3 m) where
  return :: (Monad m) => a -> Arr3T e1 e2 e3 m a
  return = pure

  (>>=) :: (Monad m) => Arr3T e1 e2 e3 m a -> (a -> Arr3T e1 e2 e3 m b) -> Arr3T e1 e2 e3 m b
  (>>=) ma k = Arr3T $ \e1 e2 e3 -> do
    a <- getArr3T ma e1 e2 e3
    getArr3T (k a) e1 e2 e3

instance (MonadFail m) => MonadFail (Arr3T e1 e2 e3 m) where
  fail :: (MonadFail m) => String -> Arr3T e1 e2 e3 m a
  fail str = Arr3T $ \_ _ _ -> fail str
