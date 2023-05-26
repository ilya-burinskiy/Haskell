newtype Odd = Odd Integer deriving (Eq, Show)

instance Enum Odd where
  succ (Odd odd) = Odd (odd + 2)
  pred (Odd odd) = Odd (odd - 2)

  -- toEnum val = Odd $ fromIntegral val
  -- fromEnum (Odd odd) = fromInteger odd

  {- [n..] -}
  enumFrom odd = odd : enumFrom (succ odd)

  {- [n, m..] -}
  enumFromThen odd@(Odd n) odd'@(Odd n') =
    let
      worker :: Odd -> [Odd]

      step = n' - n
      worker odd@(Odd n) = odd : worker (Odd (n + step))
    in odd : odd' : worker (Odd (n' + step))

  {- [n..m] -}
  enumFromTo odd@(Odd n) odd'@(Odd n')
    | n <= n' = odd : enumFromTo (succ odd) odd'
    | otherwise = []

  {- [n, m..p] -}
  enumFromThenTo start@(Odd start') next@(Odd next') end@(Odd end') = map Odd [start', next'..end']
