import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import Data.Char (toUpper)

type MyRWT m a = ReaderT [String] (Writer String) (m a)

runMyRWT :: ReaderT r m a -> r -> m a
runMyRWT = runReaderT

myAsks :: Monad m => ([String] -> m a) -> MyRWT m a
myAsks = asks

myTell :: Monad m => String -> MyRWT m ()
myTell = fmap return . tell

myLift :: Monad m => m a -> ReaderT [String] (Writer String) (m a)
myLift m = ReaderT (m >>= \ x -> return (return x))

logFirstAndRetSecond = do
  el1 <- myAsks head
  --myLift $ putStrLn $ "First is " ++ show el1
  el2 <- asks (map toUpper . head . tail)
  --myLift $ putStrLn $ "Second is " ++ show el2
  tell el1
  return el2

{-
myAsks :: ([String] -> a) -> MyRW a
myTell :: String -> ReaderT [String] (Writer String) ()
-}
