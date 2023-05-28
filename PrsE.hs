{-# LANGUAGE LambdaCase #-}
import Control.Applicative (Alternative)

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
charE :: Char -> PrsE Char

satisfyE predicate = PrsE $ \case
  char : strRem ->
    if predicate char then Right (char, strRem)
    else Left $ "unexpected " ++ [char]
  ""            -> Left "unexpected end of input"

instance Functor PrsE where
  fmap f parser = PrsE $ \str -> do
    (val, strRem) <- runPrsE parser str
    return (f val, strRem)

instance Applicative PrsE where
  pure x = PrsE $ \str -> Right (x, str)
  parserF <*> parser = PrsE $ \str -> do
    (f, strRem) <- runPrsE parserF str
    (val, strRem') <- runPrsE parser strRem
    return (f val, strRem')

instance Monad PrsE where
  m >>= k = PrsE $ \str ->
    case runPrsE m str of
      (Right (result, strRem)) -> runPrsE (k result) strRem
      (Left error) -> Left error

charE c = satisfyE (== c)
