data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Node left right) = max (height left) (height right) + 1
height (Leaf _) = 0

size :: Tree a -> Int
size (Node left right) = size left + size right + 1
size (Leaf _) = 1
