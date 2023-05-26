evenOnly :: [a] -> [a]

evenOnly = foldr (\ (idx, element) product -> if even idx then element : product else product ) [] . zip [1..]
