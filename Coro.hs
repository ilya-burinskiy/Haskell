import Control.Monad.Trans.Cont (runContT, ContT, callCC)
import Control.Monad.Writer

{-
 - callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
 - continue :: a -> Cont r b
 - yield :: ContT () (Writer String) ()
 - runContT coro2 :: (Writer String () -> ()) -> ()
 - -}
coro1 :: ContT () (Writer String) ()
coro1 = do
    callCC $ \continue -> coro2 continue
    lift $ tell "1 "
    lift $ tell "3 "
    lift $ tell "5"
    return ()

coro2 :: (() -> ContT () (Writer String) ())
         -> ContT () (Writer String) ()
coro2 yield = do
    lift $ tell "2 "
    lift $ tell "4 "
    yield ()
    lift $ tell "6"
    return ()

-- runCoros :: ContT () (Writer String) ()
--             -> Cont () (Writer String) ()
--             -> String
-- runCoros coro1 coro2 = coro1 coro
    

-- main :: IO ()
-- main = print $ execWriter $ runContT coro1 coro2 (\_ -> tell "")
