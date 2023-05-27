data Tree a = Leaf a | Node (Tree a) (Tree a)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int, Int)
    go (Leaf val) = (1, val)
    go (Node left right) =
        let (leftSubtreeNodeCount, leftSubtreeValSum) = go left
            (rightSubtreeNodeCount, rigthtSubtreeValSum) = go right
        in (leftSubtreeNodeCount + rightSubtreeNodeCount, leftSubtreeValSum + rigthtSubtreeValSum)
