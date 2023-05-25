groupElems :: Eq a => [a] -> [[a]]

groupElems [] = []
groupElems list =
  let
    helper :: Eq a => [a] -> [[a]] -> [[a]]

    helper [x] res = res
    helper (x : y : elems) res
      | x == y    = helper (y : elems) $ (y : head res) : tail res
      | otherwise = helper (y : elems) ([y] : res)
  in reverse (helper list [[head list]])
