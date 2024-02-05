import Control.Monad.RWS (Sum (Sum, getSum))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Lazy (State, StateT, evalState, evalStateT, get, modify, put, runState, state)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Show)

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (go tree) 1
  where
    go :: Tree () -> State Integer (Tree Integer)
    go (Leaf _) = state $ \num -> (Leaf num, num + 1)
    go (Fork left _ right) = state $ \num ->
      let (numberedLeft, rootNum) = runState (go left) num
          (numberedRight, nextNodeNum) = runState (go right) (rootNum + 1)
       in (Fork numberedLeft rootNum numberedRight, nextNodeNum)

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)
  where
    go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
    go (Leaf _) = do
      lift $ tell (Sum 1)
      num <- get
      modify (+ 1)
      return (Leaf num)
    go (Fork left _ right) = do
      numberedLeft <- go left
      num <- get
      modify (+ 1)
      numberedRight <- go right
      return (Fork numberedLeft num numberedRight)
