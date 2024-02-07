import Control.Applicative (liftA2)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Trans.Class (MonadTrans, lift)

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

write2log :: (Monad m) => String -> LoggT m ()
write2log str = LoggT (return (Logged str ()))

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT
