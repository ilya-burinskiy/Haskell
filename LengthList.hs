lengthList :: [a] -> Int

lengthList = foldr (\_ acc -> acc + 1) 0
