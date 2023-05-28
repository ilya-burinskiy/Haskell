data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f (Branch left value right) = Branch (fmap f left) (f value) (fmap f right)
  fmap _ Nil = Nil

instance Foldable Tree where
  foldr f ini Nil = ini
  foldr f ini tree =
    let foldrHelper [] = ini
        foldrHelper ((Branch l@(Branch {}) x r@(Branch {})) : nodes) = x `f` foldrHelper (nodes ++  [l, r])
        foldrHelper ((Branch l@(Branch {}) x Nil) : nodes) = x `f` foldrHelper (nodes ++ [l])
        foldrHelper ((Branch Nil x r@(Branch {})) : nodes) = x `f` foldrHelper (nodes ++ [r])
        foldrHelper ((Branch Nil x Nil) : nodes) = x `f` foldrHelper nodes
    in foldrHelper [tree]

instance Traversable Tree where
  traverse _ Nil = pure Nil
  traverse f (Branch left value right) = Branch <$> traverse f left
                                                <*> f value
                                                <*> traverse f right
