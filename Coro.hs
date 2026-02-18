import Control.Monad.Trans.Cont (runContT, ContT(..), callCC)
import Control.Monad.Writer
import Control.Monad.State

coro1 :: MonadWriter String m =>
         (Maybe (ContT r m ()) -> ContT r m ())
         -> ContT r m (Maybe (ContT r m ()))
coro1 yield = do
    lift $ tell "1"
    callCC $ \resume -> yield $ (Just $ resume ())
    lift $ tell "2"
    return $ Nothing

coro2 :: MonadWriter String m =>
         (Maybe (ContT r m ()) -> ContT r m ())
         -> ContT r m (Maybe (ContT r m ()))
coro2 yield = do
    lift $ tell "a"
    callCC $ \resume -> yield (Just $ resume ())
    lift $ tell "b"
    return $ Nothing

runCoros = (execWriter . (`runContT` (\_ -> tell ""))) $
    callCC coro1 >>= \mbCoro1Resume ->
        case mbCoro1Resume of
            Just coro1Resume ->
                callCC coro2 >>= \mbCoro2Resume ->
                    case mbCoro2Resume of
                        Just coro2Resume -> coro1Resume >> coro2Resume
                        Nothing -> return ()
            Nothing -> return ()
