change :: (Ord a, Num a) => a -> [[a]]

coins = [1, 5, 10, 25, 50]

change x
  | x < head coins = []
  | otherwise = filter (\changes -> sum changes == x) [coin : changes | coin <- coins, changes <- [] : change (x - coin)]
