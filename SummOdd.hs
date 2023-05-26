sumOdd :: [Integer] -> Integer
sumOdd = foldr (\ x s -> if even x then s else s + x) 0
