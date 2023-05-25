fibonacci' :: Integer -> Integer -> Integer -> Integer
fibonacci :: Integer -> Integer

fibonacci' a b n
  | n == 0 = a
  | n == 1 = b
  | n > 0 = fibonacci' b (a + b) (n - 1)
  | n < 0 = fibonacci' b (a - b) (n + 1)

fibonacci n = fibonacci' 0 1 n
