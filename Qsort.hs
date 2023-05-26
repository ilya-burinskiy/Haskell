qsort :: Ord a => [a] -> [a]

qsort [] = []
qsort [x] = [x]
qsort xs@(x:xs') =
  let
    left = filter (< x) xs
    right = filter (>= x) xs'
  in qsort left ++ x : qsort right
