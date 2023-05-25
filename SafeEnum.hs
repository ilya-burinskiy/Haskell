class (Enum a, Eq a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
    | maxBound == x = minBound
    | otherwise     = toEnum (fromEnum x + 1)

  spred :: a -> a
  spred x
    | minBound == x = maxBound
    | otherwise     = toEnum (fromEnum x - 1)
