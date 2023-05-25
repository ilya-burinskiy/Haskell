sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
  | x == 0 = (0, 1)
  | otherwise = let
    helper num digitsSum digitsCnt
      | num == 0 = (digitsSum, digitsCnt)
      | otherwise = helper (num `div` 10 ) (digitsSum + num `mod` 10) (digitsCnt + 1)
    in helper (abs (-x)) 0 0
