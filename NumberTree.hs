import Control.Monad

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  p >>= k = State $ \ s0 ->
   let (x, s1) = runState p s0
   in runState (k x) s1

numberTree' :: Tree () -> State Integer (Tree Integer)

numberTree' (Leaf _) = State $ \num -> (Leaf num, num + 1)
numberTree' (Fork left _ right) = State $ \num ->
  let (numberedLeft, rootNum) = runState (numberTree' left) num
      (numberedRight, nextNodeNum) = runState (numberTree' right) (rootNum + 1)
  in (Fork numberedLeft rootNum numberedRight, nextNodeNum)

numberTree :: Tree () -> Tree Integer
numberTree tree = fst $ runState (numberTree' tree) 1
