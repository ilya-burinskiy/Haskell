import Data.Traversable (foldMapDefault, fmapDefault)

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap = fmapDefault

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  sequenceA Nil = pure Nil
  sequenceA (Branch left value right) =
    let leftResult = sequenceA left
        rightResult = sequenceA right
    in flip . Branch <$> leftResult <*> rightResult <*> value
