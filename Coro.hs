import Control.Monad.Trans.Cont (runContT, ContT(..), callCC)
import Control.Monad.Writer

coro1 :: ((ContT r (Writer String) ())
          -> ContT r (Writer String) ())
         -> ContT r (Writer String) (ContT r (Writer String) ())
coro1 yield = do
    lift $ tell "1"
    callCC $ \resume -> yield (resume ())
    lift $ tell "2"
    return $ ContT $ \c -> c ()

coro2 :: ((ContT r (Writer String) ())
          -> ContT r (Writer String) ())
         -> ContT r (Writer String) (ContT r (Writer String) ())
coro2 yield = do
    lift $ tell "a"
    lift $ tell "b"
    callCC $ \resume -> yield (resume ())
    lift $ tell "c"
    return $ ContT $ \c -> c ()

runCoros = (execWriter . (`runContT` (\_ -> tell ""))) $ do
    coro1Cont <- callCC coro1
    coro2Cont <- callCC coro2
    coro1Cont
    coro2Cont
