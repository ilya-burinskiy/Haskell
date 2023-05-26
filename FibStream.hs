fibStream :: [Integer]
fibStream = let fibonacci a b = a : fibonacci b (a + b) in fibonacci 0 1
