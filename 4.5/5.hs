infixl 6 :+:
infixl 7 :*:

data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) =
  case (expand e1, expand e2) of
    (a :+: b, c) -> expand (a :*: c) :+: expand (b :*: c)
    (c, a :+: b) -> expand (c :*: a) :+: expand (c :*: b)
    (a, b)       -> a :*: b
expand e = e