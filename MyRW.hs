import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Char (toUpper)
import Control.Monad.Trans (MonadTrans (lift))

type MyRW = ReaderT [String] (Writer String)

myAsks :: ([String] -> a) -> MyRW a
myTell :: String -> ReaderT [String] (Writer String) ()

logFirstAndRetSecond' :: ReaderT [String] (Writer String) String
logFirstAndRetSecond' = do
  el1 <- asks head
  el2 <- asks (map toUpper . head . tail)
  lift $ tell el1
  return el2

myAsks = asks
myTell = lift . tell
