{-# LANGUAGE LambdaCase #-}

import Data.Text.Internal.Read (IParser(runP))
import Control.Applicative (Alternative, empty, (<|>))

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap f parser = Prs $ \str -> do
    (val, strRem) <- runPrs parser str
    return (f val, strRem)

instance Applicative Prs where
  pure val = Prs $ \str -> Just (val, str)
  pf <*> pv = Prs $ \str -> do
    (f, strRem) <- runPrs pf str
    (val, strRem') <- runPrs pv strRem
    return (f val, strRem')

instance Alternative Prs where
  empty = Prs $ const Nothing
  p1 <|> p2 = Prs $ \str ->
    case runPrs p1 str of
      Just (val, strRem) -> Just (val, strRem)
      Nothing            -> runPrs p2 str


anyChr :: Prs Char
anyChr = Prs $ \case
    char : tail -> Just (char, tail)
    _           -> Nothing
