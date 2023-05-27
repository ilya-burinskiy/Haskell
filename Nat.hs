data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc (toNat $ n - 1)

add :: Nat -> Nat -> Nat
add n1 n2 = toNat (fromNat n1 + fromNat n2)

mul :: Nat -> Nat -> Nat
mul n1 n2 = toNat (fromNat n1 * fromNat n2)

fac :: Nat -> Nat
fac Zero = Suc Zero
fac suc@(Suc suc') = suc `mul` fac suc'
