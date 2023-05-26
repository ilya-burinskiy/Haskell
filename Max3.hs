max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> max (max x y) z)
