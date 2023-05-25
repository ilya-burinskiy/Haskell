import GHC.Float

avg :: Int -> Int -> Int -> Double
avg a b c =
    let
        da = int2Double a
        db = int2Double b
        dc = int2Double c
    in (da + db + dc) / 3
