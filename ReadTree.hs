import Control.Monad.RWS (Sum (Sum), getSum)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Foldable (traverse_)
import Data.Traversable (fmapDefault, foldMapDefault)
import Text.Read (readMaybe)

data ReadError = EmptyInput | NoParse String deriving (Show)

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap = fmapDefault

instance Foldable Tree where
  foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Fork left value right) =
    Fork
      <$> traverse f left
      <*> f value
      <*> traverse f right

treeSum :: Tree String -> (Maybe ReadError, Integer)
treeSum t =
  let (err, s) = runWriter . runExceptT $ traverse_ go t
   in (maybeErr err, getSum s)
  where
    maybeErr :: Either ReadError () -> Maybe ReadError
    maybeErr = either Just (const Nothing)

go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
go "" = throwE EmptyInput
go str = do
  case readMaybe str :: Maybe Integer of
    Nothing -> throwE $ NoParse str
    (Just number) -> do
      lift $ tell (Sum number)
      return ()
