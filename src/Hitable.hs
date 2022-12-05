{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Hitable where 

import Data.List
import Linear.V2 (V2(..))

import Global

--types
type Wall = Coord

type Stone = Coord

type Base = [Coord]

initWalls :: [Wall]
initWalls = do
  let aTop = height - 2
  let aBottom = height - 9
  let aRight = 15
  let aLeft = 5
  let bTop = height - aBottom - 1
  let bBottom = height - aTop - 1
  let bLeft = width - aRight - 1
  let bRight = width - aLeft - 1
  let collectionA = [V2 x y | x <- [aLeft..aRight], y <- [aBottom..aTop]] \\ [V2 x y | x <- [aLeft+1..aRight-1], y <- [aBottom+1..aTop-1]]
  let collectionB = [V2 x y | x <- [bLeft..bRight], y <- [bBottom..bTop]] \\ [V2 x y | x <- [bLeft+1..bRight-1], y <- [bBottom+1..bTop-1]]
  let positions = collectionA ++ collectionB
  positions

initStones :: [Stone]
initStones = do
  let aTop = height - 4
  let aBottom = height - 6
  let aRight = 8
  let aLeft = 7
  let bTop = height - aBottom - 1
  let bBottom = height - aTop - 1
  let bLeft = width - aRight - 1
  let bRight = width - aLeft - 1
  let collectionA = [V2 x y | x <- [aLeft..aRight], y <- [aBottom..aTop]] \\ [V2 x y | x <- [aLeft+1..aRight-1], y <- [aBottom+1..aTop-1]]
  let collectionB = [V2 x y | x <- [bLeft..bRight], y <- [bBottom..bTop]] \\ [V2 x y | x <- [bLeft+1..bRight-1], y <- [bBottom+1..bTop-1]]
  let positions = collectionA ++ collectionB
  positions

initBase :: Role -> Base
initBase SelfRole = [V2 (width - 2) (height - 2), V2 (width - 2) (height - 3), V2 (width - 3) (height - 2), V2 (width - 3) (height - 3)]
initBase EnemyRole = [V2 1 1, V2 1 2, V2 2 1, V2 2 2]