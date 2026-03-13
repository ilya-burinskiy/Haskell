newtype Maybe' a = Maybe' {
    getMaybe :: Maybe a
} deriving (Eq, Show)

{-
 - Just x <> Just y = Just $ x <> y
 - Just x <> Nothing = 
 - -}
instance Semigroup a => Semigroup (Maybe' a) where
    (<>) (Maybe' (Just x)) (Maybe' (Just y)) = Maybe' $ Just $ x <> y
    (<>) _ nothing@(Maybe' Nothing) = nothing
    (<>) nothing@(Maybe' Nothing) _ = nothing

instance Monoid a => Monoid (Maybe' a) where
    mempty :: Maybe' a
    mempty = Maybe' $ Just mempty

    mappend :: Maybe' a -> Maybe' a -> Maybe' a
    mappend = (<>)
