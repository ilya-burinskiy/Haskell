{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Coroutine where

import Control.Monad (ap, forever)

{-
 - Done ndicates that we have finished computation
 - Out ueans that we should output a value but then continue the computation
 - In means that we wait for an input to be provided before carrying on the computation
 -}
data Command r u d a =
    Done a
    | Out d (Coroutine r u d a)
    | In (u -> Coroutine r u d a)
    deriving Functor

newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)


-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

instance Applicative (Coroutine r u d) where
  pure = return
  (<*>) = ap

instance Monad (Coroutine r u d) where
  return :: a -> Coroutine r u d a
  return = pure

  (>>=) :: Coroutine r u d a -> (a -> Coroutine r u d b) -> Coroutine r u d b
  {- Works only with Done -}
  (Coroutine coro) >>= k = Coroutine $ \cont -> coro $ \(Done x) -> runCoroutine (k x) cont

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
x@(Coroutine xc) >>> y@(Coroutine yc) = Coroutine $ \cont -> yc $
  \case
    (Done x) -> cont (Done x)
    (Out d rest) -> cont $ Out d $ Coroutine $ \cont -> runCoroutine (x >>> rest) cont
    (In callback) -> xc $
      \case
        (Done x) -> cont (Done x)
        (Out d rest) -> runCoroutine (rest >>> callback d) cont
        (In callback) -> cont $ In $ \u -> callback u >>> y

output :: a -> Coroutine r u a ()
output a = Coroutine $ \cont -> cont (Out a (Coroutine $ \cont' -> cont' (Done ())))

input :: Coroutine r u d u
input = Coroutine $ \cont -> cont $ In (\u -> Coroutine $ \cont' -> cont' (Done u))

produce :: [a] -> Coroutine r u a ()
produce [] = Coroutine $ \cont -> cont (Done ())
produce (x : xs) = Coroutine $ \cont -> cont $ Out x (produce xs)

consume :: Coroutine [t] u t a -> [t]
consume (Coroutine coro) = coro $ \case
  Out d rest -> d : consume rest
  Done _ -> []

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = Coroutine $ \cont ->
  cont $ In $ \u ->
    if p u
      then Coroutine $ \c -> c $ Out u (filterC p)
      else filterC p

limit :: Int -> Coroutine r v v ()
limit n
  | n < 0 = Coroutine $ \cont -> cont $ Done ()
  | n == 0 = Coroutine $ \cont -> cont $ Done ()
  | otherwise = Coroutine $ \cont ->
      cont $ In $ \u ->
        Coroutine $ \cont -> cont $ Out u (limit (n - 1))

suppress :: Int -> Coroutine r v v ()
suppress 0 = Coroutine $ \cont ->
  cont $ In $ \u ->
    Coroutine $ \cont -> cont $ Out u (suppress 0)
suppress n = Coroutine $ \cont -> cont $ In $ \_ -> suppress (n - 1)

add :: Coroutine r Int Int ()
add = Coroutine $ \cont ->
  cont $ In $ \x ->
    Coroutine $ \cont ->
      cont $ In $ \y ->
        Coroutine $ \cont -> cont $ Out (x + y) add

duplicate :: Coroutine r v v ()
duplicate = Coroutine $ \cont ->
  cont $ In $ \u ->
    Coroutine $ \cont ->
      cont $ Out u (Coroutine (\cont -> cont $ Out u duplicate))

p1 :: Coroutine r Int Int ()
p1 = filterC even >>> limit 5

p2 :: Coroutine r Int Int ()
p2 =
  let producerCoro = Coroutine $ \cont ->
        cont $ In $ \x -> Coroutine $ \cont ->
          cont $ Out ((x * (x + 1)) `div` 2) producerCoro
   in produce [1, 2 ..] >>> producerCoro

p3 :: Coroutine r Int Int ()
p3 = Coroutine $ \cont ->
  cont $ In $ \x -> Coroutine $ \cont ->
    cont $ Out (2 * x) p3

p4 :: Coroutine r Int Int ()
p4 =
  let rest prev = Coroutine $ \cont ->
        cont $ In $ \u -> Coroutine $ \cont ->
          cont $ Out (u + prev) (rest u)
   in Coroutine $ \cont ->
        cont $ In $ \x -> Coroutine $ \cont ->
          cont $ In $ \y -> Coroutine $ \cont ->
            cont $ Out (x + y) (rest y)
