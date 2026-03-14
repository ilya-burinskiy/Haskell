newtype Validate e a = Validate { getValidate :: Either [e] a }

{-
 - Классы типов Alternative и MonadPlus
 -
 - class Applicative f => Alternative f where
 -     empty :: f a
 -     (<|>) :: f a -> f a -> f a
 -
 - class (Alternative m, Monad m) => MonadPlus m where
 -     mzero :: m a
 -     mzero = empty
 -     mplus :: m a -> m a -> m a
 -     mplus = (<|>)
 -
 -  instance Alternative Maybe where
 -     empty = Nothing
 -     Nothing <|> r = r
 -     l <|> _ = l
 -
 -  instance MonadPlus Maybe -- ничего не пишем, используем базовое определение
 -
 -  guard :: Alternative f => Bool -> f ()
 -  guard True = pure ()
 -  guard False = empty
 - -}

data ReadError = EmptyInput | NoParse String deriving Show

data SumError = SummError Int ReadError deriving Show

validateSum :: [String] -> Validate SumError Integer
validateSum = undefined
