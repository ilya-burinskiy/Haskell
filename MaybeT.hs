{-# LANGUAGE LambdaCase #-}

import Control.Monad.Trans.Class
import Control.Monad (liftM)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)  }

instance Monad m => Functor (MaybeT m) where
  fmap f m = MaybeT $
    runMaybeT m >>= \ case Nothing -> return Nothing
                           Just value -> return $ Just $ f value

instance Monad m => Applicative (MaybeT m) where
  pure = MaybeT . return . Just
  mf <*> mx = MaybeT $
    runMaybeT mf >>= \ case Nothing -> return Nothing
                            Just f ->
                              runMaybeT mx >>= \ case Nothing -> return Nothing
                                                      Just x -> return $ Just $ f x

instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT m) >>= f = MaybeT $
    m >>= \ case Nothing -> return Nothing
                 Just value -> runMaybeT $ f value

instance MonadTrans MaybeT where
  lift m = MaybeT (m >>= \ x -> return (return x))
