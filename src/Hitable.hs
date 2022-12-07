{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Hitable where

import Data.List
import Global
import Linear.V2 (V2 (..))

-- | types
type Wall = Coord
type Stone = Coord
type Lake = Coord
type Base = [Coord]

initWall :: Coord -> Wall
initWall c = c

initStone :: Coord -> Stone
initStone c = c

initLake :: Coord -> Lake
initLake c = c