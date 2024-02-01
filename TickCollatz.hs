import Control.Monad (when)
import Control.Monad.Except (ExceptT (ExceptT), lift, runExceptT)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), ask, asks, runReaderT)
import Control.Monad.Trans.State (State, StateT, execState, execStateT, get, put, runState, runStateT)

type EsSi a = ExceptT String (State Integer) a

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi esSi = runState (runExceptT esSi)

type RiiEsSit m a = ReaderT (Integer, Integer) (ExceptT String (StateT Integer m)) a

runRiiEsSiT ::
  ReaderT (Integer, Integer) (ExceptT String (StateT Integer m)) a ->
  (Integer, Integer) ->
  Integer ->
  m (Either String a, Integer)
runRiiEsSiT riiEsSit bounds = runStateT (runExceptT (runReaderT riiEsSit bounds))

go' :: (Monad m) => StateT Integer m Integer -> RiiEsSit m ()
go' s = do
  (lower, upper) <- ask
  lift $ lift s
  n <- lift $ lift get
  when (n <= lower) $ lift $ throwE "Lower bound"
  when (n >= upper) $ lift $ throwE "Upper bound"

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ print res
  put res
  return n

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
