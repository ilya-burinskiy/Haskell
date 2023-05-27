local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \r -> runReader m (f r)
