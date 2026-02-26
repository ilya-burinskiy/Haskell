{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative
import Control.Monad.Writer (Writer)
import Control.Monad.Writer.Class (MonadWriter, tell, listen, pass)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Cont (runContT, mapContT, ContT(..))
import Control.Monad.Cont.Class (MonadCont, callCC)
import Control.Monad.Trans.State (StateT, mapStateT, evalStateT, get, put)

newtype CoroutineT m a = CoroutineT {
    runCoroutineT :: ContT () (StateT [CoroutineT m ()] m) a
}

instance Functor (CoroutineT m) where
    fmap f (CoroutineT cont) = CoroutineT $ fmap f cont

instance Applicative (CoroutineT m) where
    pure = CoroutineT . pure
    (<*>) (CoroutineT af) (CoroutineT av) = CoroutineT $ af <*> av

instance Monad (CoroutineT m) where
    return = pure
    (>>=) (CoroutineT mv) f = CoroutineT $ mv >>= (\x -> runCoroutineT (f x))

instance MonadTrans CoroutineT where
    lift :: Monad m => m a -> CoroutineT m a
    lift m = CoroutineT $ lift $ lift m

instance MonadCont (CoroutineT m) where
    callCC :: ((a -> CoroutineT m b) -> CoroutineT m a) -> CoroutineT m a
    callCC f = CoroutineT $ callCC $ (\exit -> runCoroutineT $ f (\a -> CoroutineT (exit a)))

instance (MonadWriter w m) => MonadWriter w (CoroutineT m) where
    tell :: w -> CoroutineT m ()
    tell w = CoroutineT $ lift $ lift $ tell w
    listen = undefined
    pass = undefined

{-
 - CoroutineT :: Cont () (StateT [CoroutineT m ()] m) a -> CoroutineT m a
 - get :: Monad m                  => StateT s m s
 - lift :: (MonadTrans t, Monad m) => m a -> t m a
 - lift get :: MonadTrans t        => t (StateT s m) s
 - CoroutineT $ lift get :: ContT () (StateT [CoroutineT m ()] m) [CoroutineT m a]
 -                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^
 -                                               m                       a
 - -}
getCCs :: Monad m => CoroutineT m [CoroutineT m ()]
getCCs = CoroutineT $ lift get

putCCs :: Monad m => [CoroutineT m ()] -> CoroutineT m ()
putCCs coros = CoroutineT $ lift (put coros)

dequeue :: Monad m => CoroutineT m ()
dequeue = do
    coros <- getCCs
    case coros of
        [] -> return ()
        (coro : coros) -> do
            putCCs coros
            coro

queue :: Monad m => CoroutineT m () -> CoroutineT m ()
queue coro = do
    coros <- getCCs
    putCCs (coros ++ [coro])

yield :: Monad m => CoroutineT m ()
yield = callCC $ \resume -> do
    queue (resume ()) 
    dequeue

fork :: Monad m => CoroutineT m () -> CoroutineT m ()
fork coro = callCC $ \resume -> do
    queue (resume ())
    coro
    dequeue

exhaust :: Monad m => CoroutineT m ()
exhaust = do
    exhausted <- null <$> getCCs
    if not exhausted
        then yield >> exhaust
        else return ()

runCoroutines :: Monad m => CoroutineT m () -> CoroutineT m () -> m ()
runCoroutines coro1 coro2 = (flip evalStateT [] . flip runContT return . runCoroutineT . (<* exhaust)) $ do
    fork coro1
    fork coro2
    
coroutine1 :: CoroutineT (Writer String) ()
coroutine1 = do
  tell "1"
  yield
  tell "2"

coroutine2 :: CoroutineT (Writer String) ()
coroutine2 = do
  tell "a"
  yield
  tell "b"

main = print $ runCoroutines coroutine1 coroutine2
