{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative (liftA2)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (ReaderT), runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask, local, reader)
import Control.Monad.State.Class (MonadState, get, put, state)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (State, StateT (StateT), modify, runState, runStateT)

class (Monad m) => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a

data Logged a = Logged String a deriving (Eq, Show)

newtype LoggT m a = LoggT {runLoggT :: m (Logged a)}

instance (Functor m) => Functor (LoggT m) where
  fmap :: (Functor m) => (a -> b) -> LoggT m a -> LoggT m b
  fmap f m = LoggT ((\(Logged log x) -> Logged log (f x)) <$> runLoggT m)

instance (Applicative m) => Applicative (LoggT m) where
  pure :: (Applicative m) => a -> LoggT m a
  pure = LoggT . pure . Logged ""

  (<*>) :: (Applicative m) => LoggT m (a -> b) -> LoggT m a -> LoggT m b
  (<*>) f a =
    LoggT $
      liftA2
        (\(Logged log f) (Logged log' x) -> Logged (log ++ log') (f x))
        (runLoggT f)
        (runLoggT a)

instance (Monad m) => Monad (LoggT m) where
  return :: (Monad m) => a -> LoggT m a
  return = pure

  (>>=) :: (Monad m) => LoggT m a -> (a -> LoggT m b) -> LoggT m b
  (>>=) m k = LoggT $ do
    (Logged log a) <- runLoggT m
    (Logged log' b) <- runLoggT (k a)
    return (Logged (log ++ log') b)

instance (MonadFail m) => MonadFail (LoggT m) where
  fail :: (MonadFail m) => String -> LoggT m a
  fail s = LoggT (fail s)

instance MonadTrans LoggT where
  lift :: (Monad m) => m a -> LoggT m a
  lift m = LoggT (fmap (Logged "") m)

instance (MonadState s m) => MonadState s (LoggT m) where
  get :: (MonadState s m) => LoggT m s
  get = lift get

  put :: (MonadState s m) => s -> LoggT m ()
  put s = lift (put s)

  state :: (MonadState s m) => (s -> (a, s)) -> LoggT m a
  state f = lift (state f)

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = LoggT . f . runLoggT

instance (MonadReader r m) => MonadReader r (LoggT m) where
  ask :: (MonadReader r m) => LoggT m r
  ask = lift ask

  local :: (MonadReader r m) => (r -> r) -> LoggT m a -> LoggT m a
  local f = mapLoggT (local f)

  reader :: (MonadReader r m) => (r -> a) -> LoggT m a
  reader f = lift (reader f)

instance (Monad m) => MonadLogg (LoggT m) where
  w2log :: (Monad m) => String -> LoggT m ()
  w2log = write2log

  logg :: (Monad m) => Logged a -> LoggT m a
  logg l = LoggT (return l)

instance (MonadLogg m) => MonadLogg (StateT s m) where
  w2log :: (MonadLogg m) => String -> StateT s m ()
  w2log = lift . w2log
  logg :: (MonadLogg m) => Logged a -> StateT s m a
  logg = lift . logg

instance (MonadLogg m) => MonadLogg (ReaderT r m) where
  w2log :: (MonadLogg m) => String -> ReaderT r m ()
  w2log = lift . w2log

  logg :: (MonadLogg m) => Logged a -> ReaderT r m a
  logg = lift . logg

write2log :: (Monad m) => String -> LoggT m ()
write2log str = LoggT (return (Logged str ()))

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

logSt'' :: LoggT (State Integer) Integer
logSt'' = do
  x <- logg $ Logged "BEGIN " 1
  lift $ modify (+ x) -- не работает без lift, но тесты прошли
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100

rdrStLog :: ReaderT Integer (StateT Integer Logg) Integer
rdrStLog = do
  x <- logg $ Logged "BEGIN " 1
  y <- ask
  lift $ modify (+ (x + y)) -- не работает без lift, но тесты прошли
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100
