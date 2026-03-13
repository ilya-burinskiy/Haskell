data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
    deriving (Eq, Show)

newtype SimpleError = Simple { getSimple :: String }
    deriving (Eq, Show)

instance Semigroup SimpleError where
    (<>) (Simple e1) (Simple e2) = Simple (e1 <> e2)

instance Monoid SimpleError where
    mempty = Simple ""
    mappend = (<>)

lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge idx) = Simple $ "[index (" ++ show idx ++ ") is too large]"
lie2se ErrNegativeIndex = Simple "[negative index]"
