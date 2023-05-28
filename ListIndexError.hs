import Control.Monad.Trans.Except

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)

(!!!) :: [a] -> Int -> Except ListIndexError a

(!!!) list i | i < 0 = throwE ErrNegativeIndex
             | otherwise =
  let
    iter idx (x : xs) | idx < i  = iter (idx + 1) xs
                      | idx == i = return x
    iter idx [] = throwE $ ErrIndexTooLarge i
  in iter 0 list
