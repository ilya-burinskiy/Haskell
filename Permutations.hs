perms :: [a] -> [[a]]
sublists :: [a] -> [[a]]

sublists [] = []
sublists [x] = [[x]]
sublists list =
  let
    listLen = length list
    indexes = [0..(listLen - 1)]
    enumeratedList = zip indexes list
  in map (\ i -> map snd (filter (\ x -> fst x /= i) enumeratedList)) indexes

perms [] = [[]]
perms [x] = [[x]]
perms list =
  let
    sublists' = sublists list
    zipped = zip list sublists'
  in concatMap (\ pair -> map (fst pair :) $ perms $ snd pair) zipped
