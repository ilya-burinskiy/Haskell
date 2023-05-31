import Control.Monad (liftM, ap, MonadPlus(mzero, mplus), guard, msum)
import Control.Applicative (Alternative(empty, (<|>)))

newtype Except e a = Except { runExcept :: Either e a } deriving (Eq, Show)
data ReadError = EmptyInput | NoParse String deriving Show
data SumError = SumError Int ReadError deriving Show

instance Functor (Except e) where
  fmap = liftM

instance Applicative (Except e) where
  pure a = Except (Right a)
  (<*>) = ap

instance Monad (Except e) where
  return = pure
  m >>= k =
    case runExcept m of
      Left e -> Except (Left e)
      Right x -> k x

instance Monoid e => Alternative (Except e) where
  empty = mzero
  (<|>) = mplus

instance Monoid e => MonadPlus (Except e) where
  mzero = Except (Left mempty)
  Except x `mplus` Except y = Except $
    case x of
      Left e -> either (Left . mappend e) Right y
      r      -> r

except :: Either e a -> Except e a
withExcept :: (e -> e') -> Except e a -> Except e' a
throwE :: e -> Except e a
catchE :: Except e a -> (e -> Except e' a) -> Except e' a

except = Except

withExcept f exception = case runExcept exception of Left error -> except $ Left (f error)
                                                     Right x -> except $ Right x

throwE = except . Left

catchE m h =
  case runExcept m of
    Left e -> h e
    Right r -> except $ Right r

tryRead :: Read a => String -> Except ReadError a
tryRead str =
  case str of
    [] -> throwE EmptyInput
    _ ->
      case reads str of
        [(parsed, strRem)] ->
          case strRem of
            "" -> return parsed
            _  -> throwE $ NoParse str
        [] -> throwE $ NoParse "wrong"

trySum :: [String] -> Except SumError Integer
trySum nums =
  fmap
    (foldr (+) 0)
    (mapM
      (\ (idx, numStr) -> withExcept (SumError idx) (tryRead numStr))
      (zip [1..] nums))
