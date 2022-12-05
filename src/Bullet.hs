{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Bullet (
    Bullet(..)
    ,bulletCoord, bulletDirection
    ,initBullet
) where

import Global
import Control.Lens hiding ((<|), (|>), (:>), (:<))


data Bullet = Bullet {
  _bulletCoord :: Coord -- bullet origin
  , _bulletDirection :: Direction
} deriving (Show)

makeLenses ''Bullet

initBullet :: Coord -> Direction -> Bullet 
initBullet c d = Bullet { _bulletCoord = c, _bulletDirection = d }

