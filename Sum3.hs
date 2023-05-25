sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 lst1 lst2 lst3 =
  let
    helper :: Num a => [a] -> [a] -> [a] -> [a] -> [a]

    helper [] (y : ys) (z : zs) res = (y + z) : helper [] ys zs res
    helper (x : xs) [] (z : zs) res = (x + z) : helper xs [] zs res
    helper (x : xs) (y : ys) [] res = (x + y) : helper xs ys [] res

    helper (x : xs) [] [] res = x : helper xs [] [] res
    helper [] (y : ys) [] res = y : helper [] ys [] res
    helper [] [] (z : zs) res = z : helper [] [] zs res

    helper [] [] [] res = res

    helper (x : xs) (y : ys) (z : zs) res = (x + y + z) : helper xs ys zs res
  in helper lst1 lst2 lst3 []
