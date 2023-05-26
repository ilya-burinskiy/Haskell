import Data.List (unfoldr)

revRange :: (Char,Char) -> [Char]

revRange (start, stop)
  | start <= stop =
    unfoldr
      (\ char ->
        let nextChar = pred char
        in if fromEnum char == fromEnum start - 1 then Nothing
           else Just (char, nextChar))
      stop
  | otherwise = []
