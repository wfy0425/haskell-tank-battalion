{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Global
  ( Tick (..),
    Role (..),
    Direction (..),
    Coord,
    Name,
    height,
    width,
    GameState (..),
  )
where

import Linear.V2 (V2 (..), _x, _y)

-- -- Constants
height, width :: Int
height = 20
width = 20

-- | Ticks mark passing of time
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
type Name = ()

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

type Coord = V2 Int

data Role = SelfRole | EnemyRole

data GameState
  = GameReady
  | GameSelecting
  | GameRunning
  | GameFinished
  | GameAborted
  deriving (Show, Eq)