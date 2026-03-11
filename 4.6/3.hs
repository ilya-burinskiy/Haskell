import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v

    fromList :: Ord k => [(k, v)] -> m k v
    fromList [] = empty
    fromList ((k, v) : xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k, v)] }
    deriving (Eq, Show)

instance MapLike ListMap where
    empty = ListMap []

    lookup k (ListMap xs) =
        let lookup' k [] = Nothing
            lookup' k ((k', v) : xs) =
                if k == k' then Just v
                    else lookup' k xs
         in lookup' k xs

    insert k v (ListMap xs) =
        let insert' k v [] = [(k, v)]
            insert' k v ((k', v') : xs) =
                -- replace old value with new
                if k == k'
                    then (k, v) : xs
                    else (k', v') : insert' k v xs

         in ListMap $ insert' k v xs

    delete k (ListMap xs) =
        let delete' k [] = []
            delete' k ((k', v') : xs) =
                if k == k'
                    then xs
                    else (k', v') : delete' k xs
         in ListMap $ delete' k xs
