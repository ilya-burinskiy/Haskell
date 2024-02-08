{-# LANGUAGE LambdaCase #-}

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)

data Tile = Floor | Chasm | Snake deriving (Show)

data DeathReason = Fallen | Poisoned deriving (Eq, Show)

type Point = (Integer, Integer)

type GameMap = Point -> Tile

map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise = Chasm

move :: GameMap -> Point -> ExceptT DeathReason [] Point
move gameMap (i, j) = do
  p <- lift [(i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)]
  case gameMap p of
    Floor -> return p
    Chasm -> throwE Fallen
    Snake -> throwE Poisoned

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves gameMap steps start =
  let loop :: GameMap -> Int -> Point -> ExceptT DeathReason [] Point
      loop _ 0 p = return p
      loop gameMap steps start = do
        p <- move gameMap start
        loop gameMap (steps - 1) p
   in runExceptT $ loop gameMap steps start

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie deathReason gameMap steps start =
  (length . filter (\case (Left d) -> d == deathReason; _ -> False)) $ moves gameMap steps start
