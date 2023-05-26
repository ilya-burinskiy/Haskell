import Data.List (unfoldr)

data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

add :: Z -> Z -> Z
mul :: Z -> Z -> Z

zToInteger :: Z -> Integer
integerToZ :: Integer -> Z

bitsToInteger :: [Bit] -> Integer
integerToBits :: Integer -> [Bit]

add a b = integerToZ $ zToInteger a + zToInteger b
mul a b = integerToZ $ zToInteger a * zToInteger b

zToInteger (Z Plus bits) = bitsToInteger bits
zToInteger (Z Minus bits) = (-1) * bitsToInteger bits

integerToZ number
    | number >= 0 = Z Plus (integerToBits number)
    | otherwise   = Z Minus (integerToBits $ abs number)

bitsToInteger =
  (.)
    (foldr
      (\ (idx, bit) product ->
        case bit of
          Zero -> product
          One  -> product + 2 ^ idx)
      0)
      (zip [0..])

integerToBits number =
    let build number
            | number == 0 = Nothing
            | otherwise   = let
                r = number `rem` 2
                k = number `div` 2
                in if r == 0 then Just (Zero, k) else Just (One, k)
    in unfoldr build number
