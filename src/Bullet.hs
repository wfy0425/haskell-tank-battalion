{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Bullet
  ( Bullet (..),
    bulletCoord,
    bulletDirection,
    tankDamage,
    enemyDamage,
    initBullet,
  )
where

import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Global

data Bullet = Bullet
  { _bulletCoord :: Coord, -- bullet origin
    _bulletDirection :: Direction,
    _tankDamage :: Int,
    _enemyDamage :: Int
  }
  deriving (Show, Eq)

makeLenses ''Bullet

initBullet :: Coord -> Direction -> Bullet
initBullet c d =
  Bullet
    { _bulletCoord = c,
      _bulletDirection = d,
      _tankDamage = 10,
      _enemyDamage = 10
    }
