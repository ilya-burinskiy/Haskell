{-# LANGUAGE LambdaCase #-}

import Control.Monad.State

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
{-
     3
    | \
   1   4
    \
    2

  foldr (:) [] tree - inorder
  [1, 2, 3, 4]

  foldr (:) [] $ PreO tree - preorder
  [3, 1, 2, 4]

  foldr (:) [] $ PostO tree - postorder
  [2, 1, 4, 3]

  foldr (:) [] $ LevelO tree - levelorder
  [3, 1, 4, 2]
  3:1:4:2:[]

  foldr f ini (LevelO Nil) = ini
  foldr f ini (LevelO (Branch l x r)) = x `f` (foldr f ini l)

  3 : foldrHelper ([] ++ [Branch Nil 1 (Branch Nil 2 Nil)] ++ [Branch Nil 4 Nil])
  3 : foldrHelper ([Branch Nil 1 (Branch Nil 2 Nil), Branch Nil 4 Nil])
  3 : (1 : foldrHelper [Branch Nil 4 Nil, Branch Nil 2 Nil])
  3 : (1 : 4 : (foldrHelper [Branch Nil 2 Nil]))
  3 : (1 : (4 : 2))
-}

instance Foldable Tree where
  foldr f ini Nil = ini
  foldr f ini (Branch l x r) = foldr f (x `f` foldr f ini r) l

instance Foldable Preorder where
  foldr f ini (PreO Nil) = ini
  foldr f ini (PreO (Branch l x r)) = x `f` foldr f (foldr f ini (PreO r)) (PreO l)

instance Foldable Postorder where
  foldr f ini (PostO Nil) = ini
  foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (x `f` ini) (PostO r)) (PostO l)

instance Foldable Levelorder where
  foldr f ini (LevelO Nil) = ini
  foldr f ini (LevelO tree) =
    let foldrHelper [] = ini
        foldrHelper ((Branch l@(Branch {}) x r@(Branch {})) : nodes) = x `f` foldrHelper (nodes ++ [l] ++ [r])
        foldrHelper ((Branch l@(Branch {}) x Nil) : nodes) =  x `f` foldrHelper (nodes ++ [l])
        foldrHelper ((Branch Nil x r@(Branch {})) : nodes) = x `f` foldrHelper (nodes ++ [r])
        foldrHelper ((Branch Nil x Nil) : nodes) = x `f` foldrHelper nodes
    in foldrHelper [tree]
