import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Data.Char (toUpper)
import Data.List (partition)

type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT t r = runWriterT (runReaderT t r)

myAsks :: (Monad m) => ([String] -> a) -> MyRWT m a
myAsks = asks

myTell :: (Monad m) => String -> MyRWT m ()
myTell = lift . tell

myLift :: (Monad m) => m a -> MyRWT m a
myLift = lift . lift

logFirstAndRetSecond' :: MyRWT IO String
logFirstAndRetSecond' = do
  el1 <- myAsks head
  myLift (putStrLn ("First is " ++ show el1))
  el2 <- myAsks (map toUpper . head . tail)
  myLift (putStrLn $ "Second is " ++ show el2)
  myTell el1
  return el1

logFirstAndRetSecond'' :: MyRWT Maybe String
logFirstAndRetSecond'' = do
  xs <- myAsks id
  case xs of
    (x1 : x2 : _) -> myTell x1 >> return (map toUpper x2)
    _ -> myLift Nothing

-- myWithReader :: (Monad m) => (r' -> r) ->  MyRWT -> Reader r
veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  xs <- myAsks id
  let (evenLenStrs, oddLenStrs) = partition (\str -> even $ length str) xs
  case evenLenStrs of
    (x1 : x2 : _) -> do
      case oddLenStrs of
        (y1 : y2 : _) -> do
          myTell $ x1 ++ "," ++ y1
          return (map toUpper x2, map toUpper y2)
        _ -> myLift Nothing
    _ -> myLift Nothing
