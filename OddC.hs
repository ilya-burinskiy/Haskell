data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

instance Functor OddC where
  fmap f (Un val) = Un (f val)
  fmap f (Bi val1 val2 rest) = Bi (f val1) (f val2) (fmap f rest)

instance Foldable OddC where
  foldr f ini (Un val) = val `f` ini
  foldr f ini (Bi val1 val2 vals) = val1 `f` (val2 `f` foldr f ini vals)

instance Traversable OddC where
  traverse f (Un val) = Un <$> f val
  traverse f (Bi val1 val2 vals) = Bi <$> f val1 <*> f val2 <*> traverse f vals
