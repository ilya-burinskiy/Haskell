{-
runCheckpointed p f = runCont (f $ \x -> Cont $ \c -> if p (c x) then c x else x) id

addTens' 1 checkpoint =
  cont $ \ c1 ->
    runCont
      (checkpoint 1)
      (\ _ ->
        (\ c2 ->
          runCont
            (checkpoint 11)
            (\ _ ->
              (\ c3 ->
                runCont
                  (checkpoint 21)
                  (\ _ -> (\ c -> c 31) c3)) c2)) c1)
-}

import Control.Monad.Cont
type Checkpointed a = (a -> Cont a a) -> Cont a a

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed predicate checkpoint = runCont
  (checkpoint $ \ x -> cont $ \ c -> if predicate (c x) then c x else x) id

addTens :: Int -> Checkpointed Int
addTens x1 checkpoint = do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2
  let x3 = x2 + 10
  checkpoint x3
  let x4 = x3 + 10
  return x4

addTens' x1 checkpoint =
  cont $ \ c1 ->
    runCont
      (checkpoint x1)
      (\ _ ->
        (\ c2 ->
          runCont
            (checkpoint (x1 + 10))
            (\ _ ->
              (\ c3 ->
                runCont
                  (checkpoint (x1 + 20))
                  (\ _ -> (\ c -> c (x1 + 30)) c3)) c2)) c1)

checkpoint x =
  let predicate = (< 30) in
  if predicate x then cont $ \ c -> c x
  else cont $ const x


{-
s >>= f = cont $ \c -> runCont s $ \x -> runCont (f x) c

sumIt = do
  a <- return 3
  b <- Cont $ \ c -> c 4 ++ c 5
  return $ a + b

sumIt =
  (return 3) >>= (\ a -> do
    b <- Cont $ \ c -> c 4 ++ c 5
    return $ a + b)

sumIt = return 3 >>= (\ a -> Cont $ \ c -> c 4 ++ c 5 >>= (\ b -> return $ a + b))
sumIt = (Cont $ \ c -> c 3) >>= (\ a -> Cont $ \ c -> c 4 ++ c 5 >>= (\ b -> Cont $ \c -> c $ a + b))
sumIt = Cont (\c -> (\ c -> c 3) (\ x -> runCont ((\ a -> Cont (\ c -> c 4 ++ c 5) >>= (\ b -> Cont $ \c -> c $ a + b)) x) c))
sumIt = Cont (\c -> (\ c -> c 3) (\ x -> runCont ((\ a -> Cont (\ c -> (\ c -> c 4 ++ c 5) (\ x -> runCont ((\ b -> Cont $ \c -> c $ a + b) x) c))) x) c))
runCont sumIt show
(\c -> (\ c -> c 3) (\ x -> runCont ((\ a -> Cont (\ c -> (\ c -> c 4 ++ c 5) (\ x -> runCont ((\ b -> Cont $ \c -> c $ a + b) x) c))) x) c)) show
(\c -> (\ c -> c 3) (\ x -> runCont ((\ a -> Cont (\ c -> (\ c -> c 4 ++ c 5) (\ x -> runCont (Cont $ \c -> c $ a + x) c))) x) c)) show
(\c1 -> (\ c2 -> c2 3) (\ x1 -> runCont ((\ a -> Cont (\ c3 -> (\ c4 -> c4 4 ++ c4 5) (\ x2 -> runCont (Cont $ \c5 -> c5 $ a + x2) c3))) x1) c1)) show

\ c3 -> (\ c4 -> c4 4 ++ c4 5) (\ x2 -> runCont (Cont $ \c5 -> c5 $ a + x2) c3)
\ c3 -> (\ x2 -> runCont (Cont $ \c5 -> c5 $ a + x2) c3) 4 ++ (\ x2 -> runCont (Cont $ \c5 -> c5 $ a + x2) c3) 5
\ c3 -> (runCont (Cont $ \c5 -> c5 $ a + 4) c3) ++ (runCont (Cont $ \c5 -> c5 $ a + 5) c3)
\ c3 -> ((\c5 -> c5 $ a + 4) c3) ++ ((\c5 -> c5 $ a + 5) c3)
\ c3 -> (c3 $ a + 4) ++ (c3 $ a + 5)

(\c1 -> (\ c2 -> c2 3) (\ x1 -> runCont ((\ a -> Cont (\ c3 -> (c3 $ a + 4) ++ (c3 $ a + 5))) x1) c1)) show
(\c1 -> (\ c2 -> c2 3) (\ x1 -> runCont (cont (\ c3 -> c3 (x1 + 4) ++ c3 (x1 + 5))) c1)) show
(\c1 -> (\ c2 -> c2 3) (\ x1 -> ((\ c3 -> c3 (x1 + 4) ++ c3 (x1 + 5))) c1)) show
(\c1 -> (\ c2 -> c2 3) (\ x1 -> (\ c3 -> c3 (x1 + 4) ++ c3 (x1 + 5)) c1)) show
(\c1 -> (\ x1 -> c1 (x1 + 4) ++ c1 (x1 + 5)) 3) show
(\c1 -> c1 (3 + 4) ++ c1 (3 + 5)) show
show (3 + 4) ++ show (3 + 5)
"78"
-}
