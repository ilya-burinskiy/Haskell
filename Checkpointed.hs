{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Use let" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
newtype Cont r a = Cont {runCont :: (a -> r) -> r}

instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f (Cont c') = Cont $ \c -> c' $ \a -> c (f a)

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure x = Cont $ \c -> c x
  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  (<*>) (Cont f) (Cont k) = Cont $ \c -> k (\a -> f (\c' -> c (c' a)))

instance Monad (Cont r) where
  return :: a -> Cont r a
  return = pure
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (>>=) (Cont c) k = Cont $ \c' -> c (\a -> runCont (k a) c')

sumIt :: Cont String Integer
sumIt =
  Cont (\c -> c 3) >>= \a ->
    Cont (\c -> c 4 ++ c 5) >>= \b ->
      return $ a + b

x' :: (Integer -> [a]) -> [a]
x' =
  \c1 ->
    (\c -> c 3)
      ( \a1 ->
          ( ( \a ->
                \c2 ->
                  (\c -> c 4 ++ c 5)
                    (\a2 -> ((\b -> \c3 -> c3 $ a + b) a2) c2)
            )
              a1 -- 3
          )
            c1 -- show
      )

{-
  самое внутренне отложенное вычислениe: \c3 -> c3 $ a + b (a и b связаны выше)
  убираем абстрагирование по b: \c3 -> c3 $ a + a2 -- a2 это результат "среднего" вычисления
  "среднее" вычисление: \c2 -> (\c -> c 4 ++ c 5) (\a2 -> (\c3 -> c3 $ a + a2) c2)
  \c2 -> (\a2 -> (\c3 -> c3 $ a + a2) c2) 4 ++ (\a2 -> (\c3 -> c3 $ a + a2) c2) 5
  \c2 -> (\c3 -> c3 $ a + 4) c2 ++ (\c3 -> c3 $ a + 5) c2

  (\a -> \c2 -> (\c3 -> c3 $ a + 4) c2 ++ (\c3 -> c3 $ a + 5) c2)
  убираем абстрагирование по a: \c2 -> (\c3 -> c3 $ a + 4) c2 ++ (\c3 -> c3 $ a1 + 5) c2
  самое верхнее вычисление
  \c1 -> (\c -> c 3) (\a1 -> (\c2 -> (\c3 -> c3 $ a + 4) c2 ++ (\c3 -> c3 $ a1 + 5) c2) c1)
  \c1 -> (\a1 -> (\c2 -> (\c3 -> c3 $ a + 4) c2 ++ (\c3 -> c3 $ a1 + 5) c2) c1) 3
  \c1 -> (\c2 -> (\c3 -> c3 $ 3 + 4) c2 ++ (\c3 -> c3 $ 3 + 5) c2) c1
-}

bind :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> (b -> r) -> r
bind v k = \c -> v (\a -> k a c)

x'' = \c ->
  (\c -> c 4 ++ c 5)
    (\a -> (\b -> (\c -> c (3 + b))) a c)

x''' = \c ->
  (\b -> (\c -> c (3 + b))) 4 c ++ (\b -> (\c -> c (3 + b))) 5 c

x = \c1 -> (\c2 -> (\c3 -> c3 $ 3 + 4) c2 ++ (\c3 -> c3 $ 3 + 5) c2) c1

type Checkpointed a = (a -> Cont a a) -> Cont a a

addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2
  let x3 = x2 + 10
  checkpoint x3
  let x4 = x3 + 10
  return x4

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed pred checkpoint =
  runCont
    ( checkpoint
        ( \x ->
            Cont
              ( \c ->
                  let futureX = c x
                   in if pred futureX
                        then futureX
                        else x
              )
        )
    )
    id

y :: Cont r Integer
y = Cont $ \k ->
  runCont
    ( do
        x <- return 10
        y <- return (x * 2)
        Cont $ \c -> k 5
        return $ x + y
    )
    k

y' :: Cont r Int
y' = Cont $ \k ->
  runCont
    ( do
        x <- return 10
        y <- return (x * 2)
        (\a -> Cont $ \c -> k a) 5
        return $ x + y
    )
    k

y'' :: Cont r Integer
y'' = Cont $ \k ->
  runCont
    ( ( \exit ->
          ( return 10 >>= \x ->
              return (x * 2) >>= \y ->
                exit 5 >>= \_ ->
                  return $ x + y
          )
      )
        (\a -> Cont $ \c -> k a)
    )
    k

y''' :: Cont r Integer
y''' =
  (\f -> Cont $ \k -> runCont (f (\a -> Cont $ \c -> k a)) k)
    ( \exit ->
        ( return 10 >>= \x ->
            return (x * 2) >>= \y ->
              exit 5 >>= \_ ->
                return $ x + y
        )
    )

callCC :: ((a1 -> Cont r a2) -> Cont r a1) -> Cont r a1
callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k

{-
( \exit ->
          ( return 10 >>= \x ->
              return (x * 2) >>= \y ->
                exit 5 >>= \_ ->
                  return $ x + y
          )
      )
square n = callCC $ \k -> k (n*n)
= Cont $ \k -> runCont ((\k -> k (n*n)) (\a -> Cont $ \_ -> k a)) k
= Cont $ \k -> runCont ((\a -> Cont $ \_ -> k a) (n*n)) k
= Cont $ \k -> runCont (Cont $ \_ -> k (n*n)) k
= Cont $ \k -> (\_ -> k (n*n)) k
= Cont $ \k -> k (n*n)

-- c first suspended computation
-- f function that accepts x :: a and returns second suspended computation
-- k :: b -> r
-- c :: (a -> r) -> r
-- (\x -> ...) :: (a -> ...) => x :: a
-- f x :: (b -> r) -> r
-- k (f x) ::
chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS c f = \k -> c (\x -> f x k)

-- cont :: (a -> ((b -> r) -> r)) -> ((a -> r) -> r)
callCC :: ((a -> ((b -> r) -> r)) -> ((a -> r) -> r))
          -> ((a -> r) -> r)
callCC f = \outerCont ->
    f (\a -> \_ignoredCont -> outerCont a) outerCont

addCPS :: Int -> Int -> ((Int -> r) -> r)
addCPS a b = \k -> k (a + b)

squareCPS :: Int -> ((Int -> r) -> r)
squareCPS x = \k -> k (x * x)

pythagorasCPS :: Int -> Int -> ((Int -> r) -> r)
pythagorasCPS x y =
    squareCPS x `chainCPS` \xx ->
    squareCPS y `chainCPS` \yy ->
    addCPS xx yy

addTens x1 = \checkpoint ->
    checkpoint x1 `chainCPS` \x1 ->
    let x2 = x1 + 10
     in checkpoint x2 `chainCPS` \x2 ->
     let x3 = x2 + 10
      in checkpoint x3 `chainCPS` \x3 ->
      let x4 = x3 + 10
       in \c -> c x4

runCheckpointed pred checkpointed =
    callCC
        (\yield -> checkpointed (\x -> if pred x then (\c -> c x) else yield x))
        id


main :: IO ()
main = do
   print (runCheckpointed (< 30) (addTens 1))
-}
