nTimes:: a -> Int -> [a]
nTimes val n =
    let
        helper lst 0 = lst
        helper lst n = helper (val : lst) (n - 1)
    in helper [] n
