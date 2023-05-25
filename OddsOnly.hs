oddsOnly :: Integral a => [a] -> [a]
oddsOnly = filter $ \x -> odd x
