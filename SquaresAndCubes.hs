merge :: [a] -> [a] -> [a]
merge [] _ = []
merge _ [] = []
merge (x:xs) (y:ys) = x:y:merge xs ys
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes nums = map (^ 2) nums `merge` map (^ 3) nums
