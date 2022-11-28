import Control.Applicative (ZipList(ZipList), getZipList)

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) f l = getZipList $ f <$> ZipList l

(>*<) :: [a -> b] -> [a] -> [b]
(>*<) fs l = getZipList $ ZipList fs <*> ZipList l
