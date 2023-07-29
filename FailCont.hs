import Control.Monad (liftM, ap)
import Except (Except, runExcept)

newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

instance Functor (FailCont r e) where
  fmap = liftM

instance Applicative (FailCont r e) where
  pure x = FailCont $ \ ok _ -> ok x
  (<*>) = ap

instance Monad (FailCont r e) where
  return = pure
  (>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b
  (FailCont v) >>= k = FailCont $ \ ok err ->
    v (\ a -> runFailCont (k a) ok err) err

toFailCont :: Except e a -> FailCont r e a
toFailCont except = FailCont $
  case runExcept except of
    (Left err) -> \ _ errCont -> errCont err
    (Right val) -> \ okCont _ -> okCont val

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont (FailCont v) = v Right Left
