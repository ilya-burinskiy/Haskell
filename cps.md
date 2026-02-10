Когда функция `f` вызывает ф-ю `g` в хвостовой позиции (tail position), то все что `f` собиралась сделать после этого
вызова - отменяется, и управление полностью переходит к `g`. Т.е. будущее вычисление `f` заменяется вычислением `g`.

```scheme
(f x) ; tail position
(+ 1 (f x)); не tail position
```

Если `f` вызывает `g` в хвостовой позиции, то `f` больше ничего не делает после вызова `g`, т.е. `g` становиться
продолжением (continuation) вычисления.

Continuation - это ф-я, которая описывает "что делать дальше с результатом"
```text
x = f(10)
print(x)
exit()
```
Continuation для `f(10)` это `\x -> print(x); exit()`

Значения типа `a` это "то же самое", что значения типа `(a -> r) -> r`. Не важно применяем ли `f` к `x` или отдаем `f`
ф-ии, которая породила `x`, и она применяет `f` к `x`

```haskell
toCPS :: a -> (forall r. (a -> r) -> r)
toCPS x = ($ x)

fromCPS :: (forall r. (a -> r) -> r) -> a
fromCPS c = c id
```

Пример вычисления факториала в CPS стиле
```haskell
factCPS :: Int -> (forall r. (Int -> r) -> r)
factCPS 0 = \c -> c 1
factCPS n = \c -> factCPS (n - 1) (\fact -> c (n * fact))
```

```haskell
foo :: Cont [r] Int
foo = do
    a <- return 3
    b <- Cont $ \c -> c 4 ++ c5
    return $ a + b
-- (... (Cont $ \c -> c 4 ++ c 5) >>= (\b -> (\c -> c $ a + b)))

foo' =
    (\c -> c 3) `chainCPS` \a ->
    (\c -> c 4 ++ c 5) `chainCPS` \b ->
    (\c -> c $ a + b)
```

