import Distribution.Simple.Utils (xargs)
data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concatOC :: OddC (OddC a) -> OddC a

concat3OC (Bi a b c) y z = Bi a b (concat3OC c y z)
concat3OC (Un a) (Bi b c d) z = Bi a b (concat3OC (Un c) d z)
concat3OC (Un a) (Un b) z = Bi a b z

concatOC (Un x) = x
concatOC (Bi x y z) = concat3OC x y (concatOC z)

instance Functor OddC where
  fmap f (Un x) = Un (f x)
  fmap f (Bi x y z) = Bi (f x) (f y) (fmap f z)

instance Applicative OddC where
  pure = Un
  (Un f) <*> (Un x) = Un (f x)
  fA@(Un f) <*> (Bi x y rest) = Bi (f x) (f y) (fA <*> rest)
  (Bi f1 f2 rest) <*> xA@(Un x) = Bi (f1 x) (f2 x) (rest <*> xA)
  (Bi f1 f2 fRest) <*> (Bi x1 x2 xRest) =
    concat3OC
      (concat3OC (Un (f1 x1)) (Un (f1 x2)) (Un f1 <*> xRest))
      (concat3OC (Un (f2 x1)) (Un (f2 x2)) (Un f2 <*> xRest))
      (concat3OC
        (fRest <*> Un x1)
        (fRest <*> Un x2)
        (fRest <*> xRest))

instance Monad OddC where
  return = pure
  (Un x) >>= k = k x
  (Bi x1 x2 xRest) >>= k = concat3OC (k x1) (k x2) (xRest >>= k)
