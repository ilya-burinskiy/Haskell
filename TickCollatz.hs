import Control.Monad (when)
import Control.Monad.Except (ExceptT (ExceptT), lift, runExceptT)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.State (State, execState, get, put, runState)

type EsSi a = ExceptT String (State Integer) a

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi esSi = runState (runExceptT esSi)

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go lower upper ticker = do
  n <- lift get
  let nextN = execState ticker n
  lift $ put nextN
  when (nextN <= lower) $ throwE "Lower bound"
  when (nextN >= upper) $ throwE "Upper bound"

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n
